extern crate proc_macro;

use proc_macro2::TokenStream;
use quote::quote;
use syn::{parenthesized, parse_macro_input};

#[proc_macro_derive(Sieve, attributes(sieve))]
pub fn derive_sieve_type(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as syn::DeriveInput);
    derive_sieve_type2(&input).into()
}

fn derive_sieve_type2(input: &syn::DeriveInput) -> TokenStream {
    match &input.data {
        syn::Data::Struct(data_struct) => match &data_struct.fields {
            syn::Fields::Named(named_fields) => {
                derive_sieve_impl(input, named_fields)
            }
            _ => {
                return quote! {
                    compile_error!("Sieve only supports named fields.");
                }
            }
        },
        _ => {
            return quote! {
                compile_error!("Sieve can only be used with structs.");
            }
        }
    }
}

fn derive_sieve_impl(input: &syn::DeriveInput, named_fields: &syn::FieldsNamed) -> TokenStream {
    let struct_name = &input.ident;
    let sift_cursor_at = derive_sieve_sift_cursor_at(input, named_fields);
    let disperse_cursor_at = derive_sieve_disperse_cursor_at(input, named_fields);
    let generated_code = quote! {
        impl sieve::SieveSift for #struct_name {
            fn sift_cursor_at(cursor: &mut std::io::Cursor<&[u8]>, offset: u64) -> Result<Self, sieve::Error> where Self: Sized {
                #sift_cursor_at
            }
        }
        impl sieve::SieveDisperse for #struct_name {
            fn disperse_cursor_at(&self, cursor: &mut std::io::Cursor<&mut [u8]>, offset: u64) -> Result<(), sieve::Error> {
                #disperse_cursor_at
            }
        }
        impl sieve::Sieve for #struct_name {
        }
    };
    generated_code
}

fn derive_sieve_sift_cursor_at(input: &syn::DeriveInput, named_fields: &syn::FieldsNamed) -> TokenStream {
    let struct_name = &input.ident;
    let global_sieve_attr = get_sieve_attr(&input.attrs).unwrap_or_default();
    let global_offset = global_sieve_attr.offset.unwrap_or(0);
    let mut current_offset: u64 = global_offset;
    let mut read_fns = Vec::<TokenStream>::with_capacity(named_fields.named.len());
    for field in named_fields.named.iter() {
        let sieve_attr = get_sieve_attr(&field.attrs).unwrap_or_default();
        let (field_type, is_option, is_vec, is_vec_option) = match derive_field_props(field) {
            Ok(args) => args,
            Err(s) => return s,
        };

        if let Some(attr_offset) = &sieve_attr.offset {
            current_offset = global_offset + attr_offset
        }

        let (read_length, read_preamble, read_function) = derive_op_function(&global_sieve_attr, &sieve_attr, &field_type, false);
        let read_count = sieve_attr.count.unwrap_or(1);
        let read_code = if is_option {
            if is_vec {
                let read_code = if is_vec_option {
                    quote! {
                        let mut results = Vec::<Option<#field_type>>::with_capacity(#read_count);
                        for n in 0..#read_count {
                            let offset = #current_offset + offset + ((n as u64) * #read_length);
                            #read_preamble
                            let result = #read_function.ok();
                            results.push(result);
                        }
                        Some(results)
                    }
                } else {
                    quote! {
                        let mut results = Vec::<#field_type>::with_capacity(#read_count);
                        let mut did_error = false;
                        for n in 0..#read_count {
                            let offset = #current_offset + offset + ((n as u64) * #read_length);
                            #read_preamble
                            if let Ok(result) = #read_function {
                                results.push(result);
                            } else {
                                did_error = true;
                                break;
                            }
                        }
                        if did_error {
                            None
                        } else {
                            Some(results)
                        }
                    }
                };
                current_offset += read_length * (read_count as u64);
                read_code
            } else {
                let full_read = quote! {
                    let offset = #current_offset + offset;
                    #read_preamble
                    #read_function.ok()
                };

                current_offset += read_length;

                full_read
            }
        } else {
            if is_vec {
                let read_code = if is_vec_option {
                    quote! {
                        let mut results = Vec::<Option<#field_type>>::with_capacity(#read_count);
                        for n in 0..#read_count {
                            let offset = #current_offset + offset + ((n as u64) * #read_length);
                            #read_preamble
                            let result = #read_function.ok();
                            results.push(result);
                        }
                        results
                    }
                } else {
                    quote! {
                        let mut results = Vec::<#field_type>::with_capacity(#read_count);
                        for n in 0..#read_count {
                            let offset = #current_offset + offset + ((n as u64) * #read_length);
                            #read_preamble
                            let result = #read_function?;
                            results.push(result);
                        }
                        results
                    }
                };
                current_offset += read_length * (read_count as u64);
                read_code
            } else {
                let use_default = match sieve_attr.default {
                    Some(use_default) => use_default,
                    None => match global_sieve_attr.default {
                        Some(use_default) => use_default,
                        None => false,
                    },
                };
                let unwrap_token = if use_default {
                    quote!(.unwrap_or_default())
                } else {
                    quote!(?)
                };
                let full_read = quote! {
                    let offset = #current_offset + offset;
                    #read_preamble
                    #read_function #unwrap_token
                };

                current_offset += read_length;

                full_read
            }
        };

        let field_name = &field.ident;
        let read_fn = quote! {
            #field_name: {
                #read_code
            },
        };
        read_fns.push(read_fn);
    };

    quote! {
        Ok(#struct_name {
            #(#read_fns)*
        })
    }
}

fn derive_sieve_disperse_cursor_at(input: &syn::DeriveInput, named_fields: &syn::FieldsNamed) -> TokenStream {
    let global_sieve_attr = get_sieve_attr(&input.attrs).unwrap_or_default();
    let global_offset = global_sieve_attr.offset.unwrap_or(0);
    let mut current_offset: u64 = global_offset;
    let mut write_fns = Vec::<TokenStream>::with_capacity(named_fields.named.len());
    for field in named_fields.named.iter() {
        let sieve_attr = get_sieve_attr(&field.attrs).unwrap_or_default();
        let (field_type, is_option, is_vec, is_vec_option) = match derive_field_props(field) {
            Ok(args) => args,
            Err(s) => return s,
        };

        if let Some(attr_offset) = &sieve_attr.offset {
            current_offset = global_offset + attr_offset
        }

        let (write_length, write_preamble, write_function) = derive_op_function(&global_sieve_attr, &sieve_attr, &field_type, true);
        let write_count = sieve_attr.count.unwrap_or(1);
        let write_code = if is_option {
            if is_vec {
                let write_code = if is_vec_option {
                    quote! {
                        if let Some(values) = value {
                            for n in 0..#write_count {
                                if let Some(value) = values.get(n).flatten() {
                                    if let Some(value) = value {
                                        let offset = #current_offset + offset + ((n as u64) * #write_length);
                                        #write_preamble
                                        #write_function?;
                                    }
                                }
                            }
                        }
                    }
                } else {
                    quote! {
                        if let Some(values) = value {
                            for n in 0..#write_count {
                                if let Some(value) = values.get(n) {
                                    let offset = #current_offset + offset + ((n as u64) * #write_length);
                                    #write_preamble
                                    #write_function?;
                                }
                            }
                        }
                    }
                };
                current_offset += write_length * (write_count as u64);
                write_code
            } else {
                let full_write = quote! {
                    if let Some(value) = value {
                        let offset = #current_offset + offset;
                        #write_preamble
                        #write_function?;
                    }
                };

                current_offset += write_length;

                full_write
            }
        } else {
            if is_vec {
                let write_code = if is_vec_option {
                    quote! {
                        let values = value;
                        for n in 0..#write_count {
                            if let Some(value) = values.get(n) {
                                if let Some(value) = value {
                                    let offset = #current_offset + offset + ((n as u64) * #write_length);
                                    #write_preamble
                                    #write_function?;
                                }
                            }
                        }
                    }
                } else {
                    quote! {
                        let values = value;
                        for n in 0..#write_count {
                            if let Some(value) = values.get(n) {
                                let offset = #current_offset + offset + ((n as u64) * #write_length);
                                #write_preamble
                                #write_function?;
                            }
                        }
                    }
                };
                current_offset += write_length * (write_count as u64);
                write_code
            } else {
                let full_write = quote! {
                    let offset = #current_offset + offset;
                    #write_preamble
                    #write_function?;
                };

                current_offset += write_length;

                full_write
            }
        };

        let field_name = &field.ident;
        let write_fn = quote! {
            {
                let value = &self.#field_name;
                #write_code
            }
        };
        write_fns.push(write_fn);
    };

    quote! {
        #(#write_fns)*
        Ok(())
    }
}

fn derive_op_function(global_sieve_attr: &SieveAttribute, sieve_attr: &SieveAttribute, field_type: &Option<&syn::Ident>, write_op: bool) -> (u64, TokenStream, TokenStream) {
    let mut field_type_str = field_type.map(|i| i.to_string()).unwrap_or("u8".to_owned());
    let try_from_result = match extract_try_from(global_sieve_attr, sieve_attr) {
        Ok(try_from) => try_from,
        Err(_) => return (0, quote!(), quote!(compile_error!("try_from must be type."))),
    };
    let mut try_from: Option<&syn::TypePath> = None;
    if let Some((f_str, t_from)) = try_from_result {
        field_type_str = f_str;
        try_from = Some(t_from);
    }
    let op_length = match field_type_str.as_str() {
        "char" => 1, "bool" => 1,
        "u8" => 1, "i8" => 1,
        "u16" => 2, "i16" => 2,
        "u32" => 4, "i32" => 4,
        "u64" => 8, "i64" => 8,
        "f32" => 4, "f64" => 8,
        _ => {
            if let Some(stride) = sieve_attr.stride {
                stride
            } else {
                let error = format!("`{}` Sieve type must be declared with `stride` attribute.", field_type_str);
                return (0, quote!(), quote!(compile_error!(#error)))
            }
        }
    };
    let byte_order = match &sieve_attr.order {
        Some(t) => quote!(#t),
        None => match &global_sieve_attr.order {
            Some(t) => quote!(#t),
            None => quote!(byteorder::LittleEndian),
        },
    };
    let op_preamble = match field_type_str.as_str() {
        "char" | "bool" | "u8" | "i8" | "u16" | "i16" | "u32" | "i32" | "u64" | "i64" | "f32" | "f64" => quote! {
            cursor.set_position(offset);
        },
        _ => {
            quote!()
        }
    };
    let op_fn = if !write_op { 
        match field_type_str.as_str() {
            "char" => quote! { 
                byteorder::ReadBytesExt::read_u8(cursor).map(|v| v as char)
            },
            "bool" => quote! { 
                byteorder::ReadBytesExt::read_u8(cursor).map(|v| v != 0)
            },
            "u8" => quote! { 
                byteorder::ReadBytesExt::read_u8(cursor)
            },
            "i8" => quote! {
                byteorder::ReadBytesExt::read_i8(cursor)
            },
            "u16" => quote! { 
                byteorder::ReadBytesExt::read_u16::<#byte_order>(cursor)
            },
            "i16" => quote! {
                byteorder::ReadBytesExt::read_i16::<#byte_order>(cursor)
            },
            "u32" => quote! {
                byteorder::ReadBytesExt::read_u32::<#byte_order>(cursor)
            },
            "i32" => quote! {
                byteorder::ReadBytesExt::read_i32::<#byte_order>(cursor)
            },
            "u64" => quote! {
                byteorder::ReadBytesExt::read_u64::<#byte_order>(cursor)
            },
            "i64" => quote! {
                byteorder::ReadBytesExt::read_i64::<#byte_order>(cursor)
            },
            "f32" => quote! {
                byteorder::ReadBytesExt::read_f32::<#byte_order>(cursor)
            },
            "f64" => quote! {
                byteorder::ReadBytesExt::read_f64::<#byte_order>(cursor)
            },
            _ => {
                quote! {
                    <#field_type as sieve::SieveSift>::sift_cursor_at(cursor, offset)
                }
            }
        }
    } else {
        match field_type_str.as_str() {
            "char" => quote! { 
                byteorder::WriteBytesExt::write_u8(cursor, *value as u8)
            },
            "bool" => quote! { 
                byteorder::WriteBytesExt::write_u8(cursor, if *value { 1 } else { 0 })
            },
            "u8" => quote! { 
                byteorder::WriteBytesExt::write_u8(cursor, *value)
            },
            "i8" => quote! {
                byteorder::WriteBytesExt::write_i8(cursor, *value)
            },
            "u16" => quote! { 
                byteorder::WriteBytesExt::write_u16::<#byte_order>(cursor, *value)
            },
            "i16" => quote! {
                byteorder::WriteBytesExt::write_i16::<#byte_order>(cursor, *value)
            },
            "u32" => quote! {
                byteorder::WriteBytesExt::write_u32::<#byte_order>(cursor, *value)
            },
            "i32" => quote! {
                byteorder::WriteBytesExt::write_i32::<#byte_order>(cursor, *value)
            },
            "u64" => quote! {
                byteorder::WriteBytesExt::write_u64::<#byte_order>(cursor, *value)
            },
            "i64" => quote! {
                byteorder::WriteBytesExt::write_i64::<#byte_order>(cursor, *value)
            },
            "f32" => quote! {
                byteorder::WriteBytesExt::write_f32::<#byte_order>(cursor, *value)
            },
            "f64" => quote! {
                byteorder::WriteBytesExt::write_f64::<#byte_order>(cursor, *value)
            },
            _ => {
                quote! {
                    sieve::SieveDisperse::disperse_cursor_at(value, cursor, offset)
                }
            }
        }
    };
    let op_fn = if let Some(try_from_type) = try_from {
        if !write_op {
            quote! {
                #op_fn.and_then(|v| <#field_type as std::convert::TryFrom<#try_from_type>>::try_from(v).map_err(|_| std::io::Error::new(std::io::ErrorKind::Other, sieve::Error::TryFromError(format!("Failed to {:?}::try_from({:?} as {:?})", stringify!(#field_type), v, stringify!(#try_from_type))))))
            }
        } else {
            quote! {
                <#try_from_type as std::convert::TryFrom<#field_type>>::try_from(*value)
                    .map_err(|_| std::io::Error::new(std::io::ErrorKind::Other, sieve::Error::TryFromError(format!("Failed to {:?}::try_from({:?} as {:?})", stringify!(#try_from_type), *value, stringify!(#field_type)))))
                    .and_then(|v| {
                        let value = &v;
                        #op_fn
                    })
            }
        }
    } else {
        op_fn
    };
    (op_length, op_preamble, op_fn)
}

fn derive_field_props(field: &syn::Field) -> Result<(Option<&syn::Ident>, bool, bool, bool), TokenStream> {
    let mut is_option = false;
    let mut is_vec = false;
    let mut is_vec_option = false;
    let field_type = match &field.ty {
        syn::Type::Path(type_path) if type_equals(&type_path, "Option") => {
            is_option = true;
            match extract_generic_type(type_path) {
                Ok(type_path) => {
                    if type_equals(&type_path, "Vec") {
                        is_vec = true;
                        match extract_generic_type(type_path) {
                            Ok(type_path) => {
                                type_path.path.segments.last().map(|s| &s.ident)
                            },
                            Err(msg) => {
                                return Err(quote!(compile_error!(format!("Unable to parse Vec type: {}", #msg))));
                            },
                        }
                    } else {
                        type_path.path.segments.last().map(|s| &s.ident)
                    }
                },
                Err(msg) => {
                    return Err(quote!(compile_error!(format!("Unable to parse Option type: {}", #msg))));
                },
            }
        },
        syn::Type::Path(type_path) if type_equals(&type_path, "Vec") => {
            is_vec = true;
            match extract_generic_type(type_path) {
                Ok(type_path) => {
                    if type_equals(&type_path, "Option") {
                        is_vec_option = true;
                        match extract_generic_type(type_path) {
                            Ok(type_path) => {
                                type_path.path.segments.last().map(|s| &s.ident)
                            },
                            Err(msg) => {
                                return Err(quote!(compile_error!(format!("Unable to parse Vec type: {}", #msg))));
                            },
                        }
                    } else {
                        type_path.path.segments.last().map(|s| &s.ident)
                    }
                },
                Err(msg) => {
                    return Err(quote!(compile_error!(format!("Unable to parse Vec type: {}", #msg))));
                },
            }
        },
        syn::Type::Path(type_path) => {
            type_path.path.segments.last().map(|s| &s.ident)
        },
        _ => {
            return Err(quote!(compile_error!(format!("Sieve: Invalid type: {#field.ty}"))));
        }
    };
    Ok((field_type, is_option, is_vec, is_vec_option))
}

fn extract_try_from<'a>(global_sieve_attr: &'a SieveAttribute, sieve_attr: &'a SieveAttribute) -> Result<Option<(String, &'a syn::TypePath)>, ()> {
    if let Some(try_from_type) = &sieve_attr.try_from {
        if let Some(type_path) = try_from_type.as_type_path() {
            Ok(Some((type_path.path.segments.last().map(|s| s.ident.to_string()).unwrap_or("u8".to_owned()), type_path)))
        } else {
            Err(())
        }
    } else if let Some(try_from_type) = &global_sieve_attr.try_from {
        if let Some(type_path) = try_from_type.as_type_path() {
            Ok(Some((type_path.path.segments.last().map(|s| s.ident.to_string()).unwrap_or("u8".to_owned()), type_path)))
        } else {
            Err(())
        }
    } else {
        Ok(None)
    }
}

trait TypeExt {
    fn as_type_path(&self) -> Option<&syn::TypePath>;
}

impl TypeExt for syn::Type {
    fn as_type_path(&self) -> Option<&syn::TypePath> {
        if let syn::Type::Path(path) = self {
            Some(path)
        } else {
            None
        }
    }
}

fn extract_generic_type(type_path: &syn::TypePath) -> Result<&syn::TypePath, String> {
    let generic_type = &type_path.path.segments.last().unwrap().arguments;
    if let syn::PathArguments::AngleBracketed(args) = generic_type {
        if args.args.len() == 1 {
            if let syn::GenericArgument::Type(ty) = args.args.first().unwrap() {
                if let syn::Type::Path(type_path) = ty {
                    Ok(type_path)
                } else {
                    Err("Unsupported type within Option.".to_owned())
                }
            } else {
                Err("Invalid type.".to_owned())
            }
        } else {
            Err("Unsupported number of generic arguments.".to_owned())
        }
    } else {
        Err("Non-parameterized type.".to_owned())
    }
}

fn type_equals(type_path: &syn::TypePath, t: &str) -> bool {
    type_path.path.segments.last().map_or(false, |segment| segment.ident == t)
}

#[derive(Default)]
struct SieveAttribute {
    offset: Option<u64>,
    stride: Option<u64>,
    count: Option<usize>,
    default: Option<bool>,
    try_from: Option<syn::Type>,
    order: Option<syn::Type>,
}

fn get_sieve_attr(attrs: &Vec<syn::Attribute>) -> Option<SieveAttribute> {
    for attr in attrs {
        if attr.path().is_ident("sieve") {
            return Some(parse_sieve_attr(&attr));
        }
    }
    None
}

fn parse_sieve_attr(attr: &syn::Attribute) -> SieveAttribute {
    let mut sieve_offset: Option<u64> = None;
    let mut sieve_stride: Option<u64> = None;
    let mut sieve_count: Option<usize> = None;
    let mut sieve_default: Option<bool> = None;
    let mut sieve_try_from: Option<syn::Type> = None;
    let mut sieve_order: Option<syn::Type> = None;
    let _ = attr.parse_nested_meta(|meta| {
        if let Some(i) = meta.path.get_ident() {
            match i.to_string().as_str() {
                "offset" => {
                    sieve_offset = parse_attr_param::<NumberAttr>(&meta).map(|a| a.content)
                }
                "stride" => {
                    sieve_stride = parse_attr_param::<NumberAttr>(&meta).map(|a| a.content)
                }
                "count" => {
                    sieve_count = parse_attr_param::<NumberAttr>(&meta).map(|a| a.content as usize)
                }
                "default" => {
                    sieve_default = parse_attr_param::<BoolAttr>(&meta).map(|a| a.content).or(Some(true))
                }
                "try_from" => {
                    sieve_try_from = parse_attr_param::<TypeAttr>(&meta).map(|a| a.content).or(Some(syn::parse_str::<syn::Type>("u8").unwrap()))
                }
                "order" => {
                    sieve_order = parse_attr_param::<TypeAttr>(&meta).map(|a| a.content)
                }
                _ => {
                    
                }
            }
        }
        Ok(())
    });
    SieveAttribute {
        offset: sieve_offset,
        stride: sieve_stride,
        count: sieve_count,
        default: sieve_default,
        try_from: sieve_try_from,
        order: sieve_order,
    }
}

struct NumberAttr {
    content: u64,
}

impl syn::parse::Parse for NumberAttr {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let content;
        parenthesized!(content in input);
        let lit: syn::LitInt = content.parse()?;
        Ok(NumberAttr {
            content: lit.base10_parse()?,
        })
    }
}

struct BoolAttr {
    content: bool,
}

impl syn::parse::Parse for BoolAttr {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let content;
        parenthesized!(content in input);
        let lit: syn::LitBool = content.parse()?;
        Ok(BoolAttr {
            content: lit.value(),
        })
    }
}

struct TypeAttr {
    content: syn::Type,
}

impl syn::parse::Parse for TypeAttr {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let content;
        parenthesized!(content in input);
        let t: syn::Type = content.parse()?;
        Ok(TypeAttr {
            content: t,
        })
    }
}

fn parse_attr_param<T>(meta: &syn::meta::ParseNestedMeta) -> Option<T> where T: syn::parse::Parse {
    if meta.input.peek(syn::token::Paren) {
        T::parse(&meta.input).ok()
    } else {
        None
    }
}
