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
    parse::{Parse, ParseStream},
    FieldsNamed, meta::ParseNestedMeta
};

#[proc_macro_derive(Sieve, attributes(sieve))]
pub fn derive_sieve_type(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let struct_name = &input.ident;

    let fields_deserialization = match &input.data {
        Data::Struct(data_struct) => match &data_struct.fields {
            Fields::Named(named_fields) => {
                derive_sieve_named_fields(struct_name, &input, named_fields)
            }
            _ => {
                return quote! {
                    compile_error!("Sieve only supports named fields.");
                }
                .into();
            }
        },
        _ => {
            return quote! {
                compile_error!("Sieve can only be used with structs.");
            }
            .into();
        }
    };

    let generated_code = quote! {
        impl sieve::Sieve for #struct_name {
            fn sift(data: &[u8], offset: u64) -> Result<Self, sieve::Error> where Self: Sized {
                #fields_deserialization
            }
        }
    };

    println!("Generated code:\n{}", generated_code.to_string());

    generated_code.into()
}

fn derive_sieve_named_fields(struct_name: &Ident, input: &DeriveInput, named_fields: &FieldsNamed) -> proc_macro2::TokenStream {
    let global_sieve_attr = get_sieve_attr(&input.attrs).unwrap_or_default();
    let mut current_offset: u64 = global_sieve_attr.offset.unwrap_or(0);
    let deserialization_code = named_fields.named.iter().map(|field| {
        let sieve_attr = get_sieve_attr(&field.attrs).unwrap_or_default();
        let mut is_primitive = false;
        let mut is_option = false;
        let field_type = match &field.ty {
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
            Type::Path(type_path) => {
                is_primitive = true;
                type_path.path.segments.last().unwrap().ident.to_string()
            }
            _ => {
                return quote! {
                    compile_error!(format!("Sieve: Invalid type: {#field.ty}"));
                }
                .into();
            }
        };

        let (read_length, read_function) = derive_read_function(&sieve_attr, &field_type);
        let read_code = if is_option {
            quote! {
                #read_function.ok()
            }
        } else {
            quote! {
                #read_function?
            }
        };

        let field_name = &field.ident;
        let field_offset = sieve_attr.offset;

        quote! {
            #field_name: {
                cursor.set_position(#field_offset + offset);
                #read_code
            },
        }
    });

    quote! {
        let mut cursor = std::io::Cursor::new(data);
        Ok(#struct_name {
            #(#deserialization_code)*
        })
    }
}

fn derive_read_function(sieve_attr: &SieveAttribute, field_type: &str) -> (u64, proc_macro2::TokenStream) {
    let read_length = match field_type {
        "bool" => 1, "u8" => 1, "i8" => 1,
        "u16" => 2, "i16" => 2,
        "u32" => 4, "i32" => 4,
        "u64" => 8, "i64" => 8,
        "f32" => 4, "f64" => 8,
        _ => {
            0
        }
    };
    let byte_order = match &sieve_attr.order {
        Some(t) => quote!(#t),
        None => quote!(byteorder::LittleEndian),
    };
    let read_fn = match field_type {
        "bool" => quote! { 
            byteorder::ReadBytesExt::read_u8(&mut cursor).map(|v| v != 0)
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
            if let Some(t) = syn::parse_str::<syn::Type>(field_type).ok() {
                let field_offset = sieve_attr.offset;
                quote! {
                    <#t as sieve::Sieve>::sift(data, #field_offset + offset)
                }
            } else {
                quote! {
                    compile_error!("Sieve: Unsupported type");
                }
            }
        }
    };
    (read_length, read_fn)
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
    offset: Option<u64>,
    stride: Option<u64>,
    count: Option<u64>,
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

fn parse_sieve_attr(attr: &Attribute) -> SieveAttribute {
    let mut sieve_offset: Option<u64> = None;
    let mut sieve_stride: Option<u64> = None;
    let mut sieve_count: Option<u64> = None;
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
                    sieve_count = parse_attr_param::<NumberAttr>(&meta).map(|a| a.content)
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
        order: sieve_order,
    }
}

struct NumberAttr {
    content: u64,
}

impl Parse for NumberAttr {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let content;
        parenthesized!(content in input);
        let lit: LitInt = content.parse()?;
        let n: u64 = lit.base10_parse()?;
        Ok(NumberAttr {
            content: n,
        })
    }
}

struct TypeAttr {
    content: syn::Type,
}

impl Parse for TypeAttr {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let content;
        parenthesized!(content in input);
        let lit: syn::Lit = content.parse()?;
        Ok(TypeAttr {
            content: syn::parse_str::<syn::Type>(&lit.into_token_stream().to_string())?,
        })
    }
}

fn parse_attr_param<T>(meta: &ParseNestedMeta) -> Option<T> where T: Parse {
    if meta.input.peek(token::Paren) {
        <T as syn::parse::Parse>::parse(&meta.input).ok()
    } else {
        None
    }
}
