use proc_macro2::*;
use proc_quote::*;
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::*;


pub fn derive(input: &DeriveInput) -> TokenStream {
    unimplemented!()

    
    // quote! {
    //     impl ::std::fmt::Display for #struct_name {
    //         fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
    //             ::std::write!(f, "{} : {}"), #filter_name, &self.args)
    //         }
    //     }
    // }
}
