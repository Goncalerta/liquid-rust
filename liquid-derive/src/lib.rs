#![recursion_limit = "128"]
extern crate liquid_compiler;
extern crate liquid_error;
extern crate proc_macro;
extern crate proc_macro2;
extern crate quote;
extern crate syn;

mod filter_parameters;
mod filter_parser;

use proc_macro::TokenStream;

#[proc_macro_derive(FilterParameters, attributes(parameter))]
pub fn derive_filter_parameters(item: TokenStream) -> TokenStream {
    let input = syn::parse_macro_input!(item as syn::DeriveInput);
    filter_parameters::derive(&input).into()
}

#[proc_macro_derive(FilterParser, attributes(filter))]
pub fn derive_filter_parser(item: TokenStream) -> TokenStream {
    let input = syn::parse_macro_input!(item as syn::DeriveInput);
    filter_parser::derive(&input).into()
}
