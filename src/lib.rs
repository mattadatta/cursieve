extern crate proc_macro;

use proc_macro::TokenStream;
use quote::{quote, ToTokens};
use syn::{
    parenthesized,
    parse_macro_input,
    token,
    LitInt,
    Data,
    DeriveInput,
    Ident,
    Field,
    Fields,
    Type,
    Attribute,
    FieldsNamed, meta::ParseNestedMeta
};

#[proc_macro_derive(SieveType, attributes(sieve))]
pub fn derive_sieve_type(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let struct_name = &input.ident;

    let fields_deserialization = match &input.data {
        Data::Struct(data_struct) => match &data_struct.fields {
            Fields::Named(named_fields) => {
                derive_sieve_named_fields(struct_name, named_fields)
            }
            _ => {
                return quote! {
                    compile_error!("SieveType only supports named fields.");
                }
                .into();
            }
        },
        _ => {
            return quote! {
                compile_error!("SieveType can only be used with structs.");
            }
            .into();
        }
    };

    let generated_code = quote! {
        impl #struct_name {
            fn sift(data: &[u8], offset: u64) -> Self {
                #fields_deserialization
            }
        }
    };

    println!("Generated code:\n{}", generated_code.to_string());

    generated_code.into()
}

fn derive_sieve_named_fields(struct_name: &Ident, named_fields: &FieldsNamed) -> proc_macro2::TokenStream {
    let deserialization_code = named_fields.named.iter().map(|field| {
        let field_name = &field.ident;
        let sieve_attr = get_sieve_attr(&field).unwrap_or_default();
        let field_offset = sieve_attr.offset;

        let mut is_primitive = false;
        let mut is_option = false;
        let field_type = match &field.ty {
            Type::Path(type_path) if is_primitive_type(&type_path) => {
                is_primitive = true;
                type_path.path.segments.last().unwrap().ident.to_string()
            }
            Type::Path(type_path) if is_option_type(&type_path) => {
                is_option = true;
                let generic_type = &type_path.path.segments.last().unwrap().arguments;
                if let syn::PathArguments::AngleBracketed(args) = generic_type {
                    if args.args.len() == 1 {
                        if let syn::GenericArgument::Type(ty) = args.args.first().unwrap() {
                            if let Some(type_name) = extract_type_name(&ty) {
                                type_name
                            } else {
                                return quote! {
                                    compile_error!("Unsupported type within Option.");
                                }
                                .into();
                            }
                        } else {
                            return quote! {
                                compile_error!("Invalid type within Option.");
                            }
                            .into();
                        }
                    } else {
                        return quote! {
                            compile_error!("Unsupported number of generic arguments within Option.");
                        }
                        .into();
                    }
                } else {
                    return quote! {
                        compile_error!("Unsupported type within Option.");
                    }
                    .into();
                }
            }
            _ => {
                return quote! {
                    compile_error!("SieveType only supports integer types or Option<...>.");
                }
                .into();
            }
        };

        let read_function = derive_read_function(&sieve_attr, &field_type);
        let read_code = if is_option {
            quote! {
                #read_function.ok()
            }
        } else {
            quote! {
                #read_function.ok().unwrap_or_default()
            }
        };

        quote! {
            #field_name: {
                cursor.set_position(#field_offset + offset);
                #read_code
            },
        }
    });

    quote! {
        let mut cursor = std::io::Cursor::new(data);
        #struct_name {
            #(#deserialization_code)*
        }
    }
}

fn derive_read_function(sieve_attr: &SieveAttribute, field_type: &str) -> proc_macro2::TokenStream {
    let byte_order = &sieve_attr.order;
    match field_type {
        "bool" => quote! { 
            byteorder::ReadBytesExt::read_u8(&mut cursor).map(|v| { return v != 0; })
        },
        "u8" => quote! { 
            byteorder::ReadBytesExt::read_u8(&mut cursor)
        },
        "i8" => quote! {
            byteorder::ReadBytesExt::read_i8(&mut cursor)
        },
        "u16" => quote! { 
            byteorder::ReadBytesExt::read_u16::<#byte_order>(&mut cursor)
        },
        "i16" => quote! {
            byteorder::ReadBytesExt::read_i16::<#byte_order>(&mut cursor)
        },
        "u32" => quote! {
            byteorder::ReadBytesExt::read_u32::<#byte_order>(&mut cursor)
        },
        "i32" => quote! {
            byteorder::ReadBytesExt::read_i32::<#byte_order>(&mut cursor)
        },
        "u64" => quote! {
            byteorder::ReadBytesExt::read_u64::<#byte_order>(&mut cursor)
        },
        "i64" => quote! {
            byteorder::ReadBytesExt::read_i64::<#byte_order>(&mut cursor)
        },
        "f32" => quote! {
            byteorder::ReadBytesExt::read_f32::<#byte_order>(&mut cursor)
        },
        "f64" => quote! {
            byteorder::ReadBytesExt::read_f64::<#byte_order>(&mut cursor)
        },
        _ => {
            return quote! {
                compile_error!("Unsupported integer type.");
            }
        }
    }
}

fn is_primitive_type(type_path: &syn::TypePath) -> bool {
    let type_name = type_path.path.segments.last().unwrap().ident.to_string();
    matches!(
        type_name.as_str(),
        "u8" | "u16" | "u32" | "u64" |
        "i8" | "i16" | "i32" | "i64" |
        "f32" | "f64" | "bool"
    )
}

fn is_option_type(type_path: &syn::TypePath) -> bool {
    if let Some(segment) = type_path.path.segments.last() {
        segment.ident == "Option"
    } else {
        false
    }
}

fn extract_type_name(ty: &Type) -> Option<String> {
    if let Type::Path(type_path) = ty {
        if let Some(segment) = type_path.path.segments.last() {
            Some(segment.ident.to_string())
        } else {
            None
        }
    } else {
        None
    }
}

#[derive(Debug, Default)]
struct SieveAttribute {
    offset: u64,
    order: proc_macro2::TokenStream,
}

fn get_sieve_attr(field: &Field) -> Option<SieveAttribute> {
    for attr in &field.attrs {
        if attr.path().is_ident("sieve") {
            return Some(parse_sieve_attr(&field, &attr));
        }
    }
    None
}

fn parse_sieve_attr(_field: &Field, attr: &Attribute) -> SieveAttribute {
    let mut sieve_offset: u64 = 0;
    let mut sieve_order: proc_macro2::TokenStream = quote! { byteorder::LittleEndian };
    let _ = attr.parse_nested_meta(|meta| {
        if let Some(i) = meta.path.get_ident() {
            match i.to_string().as_str() {
                "offset" => {
                    sieve_offset = parse_seive_attr_offset(_field, attr, &meta).unwrap_or_default()
                }
                "order" => {
                    sieve_order = parse_seive_attr_order(_field, attr, &meta).ok().unwrap_or(quote! { byteorder::LittleEndian })
                }
                _ => {
                    
                }
            }
        }
        Ok(())
    });
    SieveAttribute { 
        offset: sieve_offset,
        order: sieve_order,
     }
}

fn parse_seive_attr_offset(_field: &Field, attr: &Attribute, meta: &ParseNestedMeta) -> Result<u64, syn::Error> {
    if meta.input.peek(token::Paren) {
        let content;
        parenthesized!(content in meta.input);
        let lit: LitInt = content.parse()?;
        let n: u64 = lit.base10_parse()?;
        Ok(n)
    } else {
        Ok(0)
    }
}

fn parse_seive_attr_order(_field: &Field, attr: &Attribute, meta: &ParseNestedMeta) -> Result<proc_macro2::TokenStream, syn::Error> {
    if meta.input.peek(token::Paren) {
        let content;
        parenthesized!(content in meta.input);
        let lit: syn::Lit = content.parse()?;
        if let Some(t) = syn::parse_str::<syn::Type>(&lit.into_token_stream().to_string()).ok() {
            return Ok(quote! { #t })
        }
    }
    Ok(quote! { byteorder::LittleEndian })
}
