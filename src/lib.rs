extern crate proc_macro;

use proc_macro::TokenStream;
use quote::quote;
use syn::{parenthesized, parse_macro_input, token, LitInt, Data, DeriveInput, Field, Fields, Attribute};

#[proc_macro_derive(SieveType)]
pub fn derive_sieve_type(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let struct_name = &input.ident;

    let fields_deserialization = match &input.data {
        Data::Struct(data_struct) => match &data_struct.fields {
            Fields::Named(named_fields) => {
                let extraction_code = named_fields.named.iter().map(|field| {
                    let field_name = &field.ident;
                    let sieve_attr = get_sieve_attr(&field).unwrap_or_default();
                    let field_offset = sieve_attr.offset;
                    let field_type = &field.ty;

                    quote! {
                        cursor.set_position(#field_offset + offset);
                        let #field_name = cursor.read::<#field_type>().ok()?;
                    }
                });

                let deserialization_code = named_fields.named.iter().map(|field| {
                    let field_name = &field.ident;
                    quote! {
                        #field_name,
                    }
                });

                quote! {
                    let mut cursor = std::io::Cursor::new(data);
                    #(#extraction_code)*
                    #struct_name {
                        #(#deserialization_code)*
                    }
                }
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

#[derive(Debug, Default)]
struct SieveAttribute {
    offset: u64
}

fn get_sieve_attr(field: &Field) -> Option<SieveAttribute> {
    for attr in &field.attrs {
        if attr.path().is_ident("sieve") {
            return Some(parse_sieve_attr(&field, &attr));
        }
    }
    None
}

fn parse_sieve_attr(field: &Field, attr: &Attribute) -> SieveAttribute {
    let mut sieve_offset: u64 = 0;
    let _ = attr.parse_nested_meta(|meta| {
        if meta.path.is_ident("offset") {
            if meta.input.peek(token::Paren) {
                let content;
                parenthesized!(content in meta.input);
                let lit: LitInt = content.parse()?;
                let n: u64 = lit.base10_parse()?;
                sieve_offset = n;
            } else {
                sieve_offset = 0;
            }
        }
        Ok(())
    });
    return SieveAttribute { offset: sieve_offset }
}
