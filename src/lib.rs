//! `#[derive(ToTokens)]`, calling [`quote::ToTokens`](https://docs.rs/quote/1.0.33/quote/trait.ToTokens.html#tymethod.to_tokens) on each field.
//! Nothing more, nothing less.
//!
//! ```
//! use derive_quote_to_tokens::ToTokens;
//! use quote::{ToTokens, quote};
//! use proc_macro2::Span;
//! use syn::{Token, Ident};
//!
//! #[derive(ToTokens)]
//! struct Tag {
//!     lt: Token![<],
//!     inner: Ident,
//!     gt: Token![>],
//! }
//!
//! let tag = /* snip */
//! # Tag { lt: Token![<](Span::call_site()), inner: Ident::new("main", Span::call_site()), gt: Token![>](Span::call_site()) };
//! assert_eq!(
//!     tag.to_token_stream().to_string(),
//!     quote! { <main> }.to_string(),
//! );
//! ```
//!
//! Enums work too.
//! ```
//! # use derive_quote_to_tokens::ToTokens;
//! # use syn::Token;
//! #[derive(ToTokens)]
//! enum Arrow {
//!      Left(Token![<], Token![-]),
//!     Right(Token![-], Token![>]),
//! }
//! ```

use syn_helpers::{
    derive_trait,
    proc_macro2::Span,
    syn::{parse_macro_input, parse_quote, DeriveInput, Ident, Stmt},
    Constructable as _, FieldMut as _, Trait, TraitItem, TypeOfSelf,
};

#[proc_macro_derive(ToTokens, attributes(to_tokens))]
pub fn derive_to_tokens(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let item = parse_macro_input!(input as DeriveInput);
    derive_trait(item, to_tokens()).into()
}

fn to_tokens() -> Trait {
    Trait {
        name: parse_quote!(::quote::ToTokens),
        generic_parameters: None,
        items: vec![TraitItem::Method {
            name: Ident::new("to_tokens", Span::call_site()),
            generic_parameters: None,
            self_type: TypeOfSelf::Reference,
            other_parameters: vec![parse_quote!(tokens: &mut ::proc_macro2::TokenStream)],
            return_type: None,
            handler: Box::new(|mut item| {
                item.map_constructable(|mut constructable| {
                    Ok(constructable
                        .get_fields_mut()
                        .fields_iterator_mut()
                        .flat_map(|mut field| -> Option<Stmt> {
                            let field = field.get_reference();
                            Some(parse_quote!(::quote::ToTokens::to_tokens(#field, tokens);))
                        })
                        .collect())
                })
            }),
        }],
    }
}

#[cfg(test)]
fn do_test(input: DeriveInput, expected: syn_helpers::proc_macro2::TokenStream) {
    use pretty_assertions::assert_eq;
    use syn_helpers::syn::{parse2, ItemImpl};

    let actual = parse2::<ItemImpl>(derive_trait(input, to_tokens())).expect("invalid output");
    let expected = parse2::<ItemImpl>(expected).expect("invalid input");
    assert_eq!(&actual, &expected);
}

#[test]
fn test() {
    use syn_helpers::quote;
    do_test(
        parse_quote!(
            struct Foo {
                a: Ident,
                b: String,
            }
        ),
        quote! {
            #[automatically_derived]
            impl ::quote::ToTokens for Foo {
                fn to_tokens(&self, tokens: &mut ::proc_macro2::TokenStream) {
                    let Foo { a: ref _0, b: ref _1 } = self;
                    ::quote::ToTokens::to_tokens(_0, tokens);
                    ::quote::ToTokens::to_tokens(_1, tokens);
                }
            }
        },
    );

    do_test(
        parse_quote!(
            enum Either {
                Left(Ident),
                Right { named: Ident },
            }
        ),
        quote! {
            #[automatically_derived]
            impl ::quote::ToTokens for Either {
                fn to_tokens(&self, tokens: &mut ::proc_macro2::TokenStream) {
                    match self {
                        Either::Left(ref _0) => {
                            ::quote::ToTokens::to_tokens(_0, tokens);
                        }
                        Either::Right { named: ref _0 } => {
                            ::quote::ToTokens::to_tokens(_0, tokens);
                        }
                    }
                }
            }
        },
    );
}

#[test]
fn readme() {
    assert!(
        std::process::Command::new("cargo")
            .args(["rdme", "--check"])
            .output()
            .expect("couldn't run `cargo rdme`")
            .status
            .success(),
        "README.md is out of date - bless the new version by running `cargo rdme`"
    )
}
