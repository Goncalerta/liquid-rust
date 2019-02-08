extern crate liquid_compiler;
extern crate liquid_error;
extern crate proc_macro;
extern crate proc_macro2;
extern crate proc_quote;
extern crate syn;

mod filter_parameters;
pub(crate) mod helpers;
mod filter;
mod parse_filter;

use proc_macro::TokenStream;

#[proc_macro_derive(FilterParameters, attributes(parameter, evaluated))]
pub fn derive_filter_parameters(item: TokenStream) -> TokenStream {
    let input = syn::parse_macro_input!(item as syn::DeriveInput);
    filter_parameters::derive(&input).into()
}

#[proc_macro_derive(ParseFilter, attributes(filter))]
pub fn derive_parse_filter(item: TokenStream) -> TokenStream {
    let input = syn::parse_macro_input!(item as syn::DeriveInput);
    parse_filter::parse_filter::derive(&input).into()
}

#[proc_macro_derive(FilterReflection, attributes(filter))]
pub fn derive_filter_reflection(item: TokenStream) -> TokenStream {
    let input = syn::parse_macro_input!(item as syn::DeriveInput);
    parse_filter::filter_reflection::derive(&input).into()
}

#[proc_macro_derive(FromFilterParameters, attributes(parameters))]
pub fn derive_from_filter_parameters(item: TokenStream) -> TokenStream {
    let input = syn::parse_macro_input!(item as syn::DeriveInput);
    filter::from_filter_parameters::derive(&input).into()
}

#[proc_macro_derive(Display_filter, attributes(name, parameters))]
pub fn derive_display_filter(item: TokenStream) -> TokenStream {
    let input = syn::parse_macro_input!(item as syn::DeriveInput);
    filter::display::derive(&input).into()
}