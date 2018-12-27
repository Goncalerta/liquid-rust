#![recursion_limit = "128"]
extern crate liquid_compiler;
extern crate liquid_error;
extern crate proc_macro;
extern crate proc_macro2;
extern crate quote;
extern crate syn;

use proc_macro::TokenStream;
use quote::quote;
use syn::*;

// Probably there is a better approach that doesn't require adding this struct
// Goal is to be able to use `#` inside a `quote!` that writes all objects inside the `Vec`
struct ToTokensVec<T: quote::ToTokens>(Vec<T>);
impl<T: quote::ToTokens> quote::ToTokens for ToTokensVec<T> {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        for token in self.0.iter() {
            token.to_tokens(tokens);
        }
    }
}

// Generates the statement to construct the field with the given name inside `new(mut args: FilterArguments)`
fn generate_construct_field(ident: &Ident, is_option: bool) -> proc_macro2::TokenStream {
    if is_option {
        quote! {
            let #ident = args.positional.next();
        }
    } else {
        quote! {
            let #ident = args.positional.next().ok_or_else(|| liquid_error::Error::with_msg("Required"))?;
        }
    }
}

// Generates the statement to evaluate the field with the given name into a `Value`
fn generate_evaluate_field(ident: &Ident, is_option: bool) -> proc_macro2::TokenStream {
    if is_option {
        quote! {
            let #ident = match &self.#ident {
                Some(field) => Some(field.evaluate(context)?.to_owned()),
                None => None,
            };
        }
    } else {
        quote! {
            let #ident = self.#ident.evaluate(context)?.to_owned();
        }
    }
}

// Access the last path segment in a given Type.
// Helper function for `generate_evaluated_definition`
fn type_last_path_segment_mut(ty: &mut Type) -> &mut PathSegment {
    if let Type::Path(ty) = ty {
        let segments = &mut ty.path.segments;
        let last = segments.last_mut().expect("Has a type.").into_value();

        last
    } else {
        panic!("All fields must be of type Option<Expression> or Expression")
    }
}

// From a `field: Expression,`, generate a `field: Value,`
fn generate_evaluated_definition(field: &Field, is_option: bool) -> Field {
    let field = field.clone();
    let Field {
        attrs: _,
        vis,
        ident,
        colon_token,
        mut ty,
    } = field;
    {
        let last = type_last_path_segment_mut(&mut ty);

        if is_option {
            match last.arguments {
                PathArguments::AngleBracketed(ref mut arg) => {
                    match arg
                        .args
                        .last_mut()
                        .expect("Expected `Option<Value>`")
                        .into_value()
                    {
                        GenericArgument::Type(ref mut ty) => {
                            let mut last = type_last_path_segment_mut(ty);
                            last.ident = Ident::new("Value", proc_macro2::Span::call_site());
                        }
                        _ => panic!("Expected `Option<Value>`"),
                    }
                }
                _ => panic!("Expected `Option<Value>`"),
            }
        } else {
            last.ident = Ident::new("Value", proc_macro2::Span::call_site());
        };
    }
    Field {
        attrs: Vec::new(),
        vis,
        ident,
        colon_token, // Is it okay to share the token of the original field?
        ty,
    }
}

#[proc_macro_derive(FilterParameters)]
pub fn derive_filter_parameters(item: TokenStream) -> TokenStream {
    let input = syn::parse_macro_input!(item as syn::DeriveInput);
    let name = &input.ident;
    let evaluated_name = Ident::new(&format!("Evaluated{}", name), proc_macro2::Span::call_site());
    let data = match &input.data {
        Data::Struct(data) => data,
        _ => panic!("FilterParameters is only available to structs."),
    };
    let fields = match &data.fields {
        Fields::Named(fields) => &fields.named,
        Fields::Unnamed(fields) => panic!("Unnamed fields are not yet supported"),
        Fields::Unit => panic!("Unit structs are not yet supported"),
    };

    // TODO use with_capacity
    let mut construct_fields = Vec::new();
    let mut evaluate_fields = Vec::new();
    let mut fields_list: punctuated::Punctuated<_, Token![,]> = punctuated::Punctuated::new();
    let mut evaluated_definition = Vec::new();
    for field in fields {
        let Field {
            attrs, ident, ty, ..
        } = field;
        let ident = match ident {
            Some(ident) => ident,
            None => panic!("Must have ident; Unsupported otherwise"),
        };
        let ty = match ty {
            Type::Path(ty) => ty.path.segments.last().expect("Has a type.").into_value(),
            _ => panic!("All fields must be of type Option<Expression> or Expression"),
        };

        let is_option = ty.ident.to_string() == "Option";

        construct_fields.push(generate_construct_field(ident, is_option));
        evaluate_fields.push(generate_evaluate_field(ident, is_option));

        evaluated_definition.push(field);

        fields_list.push(ident.clone());
    }
    let construct_fields = ToTokensVec(construct_fields);
    let evaluate_fields = ToTokensVec(evaluate_fields);
    let evaluated_definition = ToTokensVec(evaluated_definition);

    let implement = quote! {

        impl #name {
            fn new(mut args: FilterArguments) -> Result<Self> {
                #construct_fields

                args.check_args_exhausted()?;
                Ok( #name { #fields_list } )
            }

            fn evaluate(&self, context: &Context) -> Result<#evaluated_name> {
                #evaluate_fields

                Ok( #evaluated_name{ #fields_list } )
            }
        }

        struct #evaluated_name {
            #evaluated_definition
        }
    };
    implement.into()
}
