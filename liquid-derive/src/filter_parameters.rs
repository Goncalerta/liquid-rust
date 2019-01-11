use proc_macro2::*;
use quote::*;
use std::borrow::Cow;
use syn::punctuated::Punctuated;
use syn::*;

// TODO pay close atention to paths (ex. liquid::error::Error::with_msg)

/// Generates the statement that saves the next positional argument in the iterator in `ident`.
fn generate_construct_field(ident: &Ident, is_option: bool) -> TokenStream {
    if is_option {
        quote! {
            let #ident = args.positional.next();
        }
    } else {
        quote! {
            // problem => doesnt work if liquid_error is not in use
            let #ident = args.positional.next().ok_or_else(|| liquid::error::Error::with_msg("Required"))?;
        }
    }
}

/// Generates the statement that evaluates the `Expression` in `ident`
fn generate_evaluate_field(ident: &Ident, is_option: bool) -> TokenStream {
    if is_option {
        quote! {
            let #ident = match &self.#ident {
                Some(field) => Some(field.evaluate(context)?),
                None => None,
            };
        }
    } else {
        quote! {
            let #ident = self.#ident.evaluate(context)?;
        }
    }
}

/// Checks whether this time is `Option<Expression>` (true) or `Expression` (false)
fn is_type_option(ty: &Type) -> bool {
    if let Type::Path(ty) = ty {
        ty.path
            .segments
            .last()
            .expect("is_type_option must be used in a valid FilterParameters struct.")
            .into_value()
            .ident
            .to_string()
            == "Option"
    } else {
        panic!("is_type_option must be used in a valid FilterParameters struct.")
    }
}

/// Helper struct for `generate_impl_filter_parameters`
struct FilterParameter<'a> {
    name: Cow<'a, Ident>,
    is_optional: bool,
}
impl<'a> ToTokens for FilterParameter<'a> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.name.to_tokens(tokens);
    }
}

/// Generates `new()` and `evaluate()` methods for the struct named `name`
fn generate_impl_filter_parameters(
    name: &Ident,
    evaluated_name: &Ident,
    fields: &Fields,
) -> TokenStream {
    let (fields, generate_constructor) = match fields {
        Fields::Named(fields) => {
            let fields: Punctuated<_, Token![,]> = fields
                .named
                .iter()
                .map(|field| {
                    let name = &field.ident.as_ref().expect("Fields are named.");
                    FilterParameter {
                        name: Cow::Borrowed(name),
                        is_optional: is_type_option(&field.ty),
                    }
                })
                .collect();
            let generate_constructor = quote! { { #fields } };

            (fields, generate_constructor)
        }

        Fields::Unnamed(fields) => {
            let fields: Punctuated<_, Token![,]> = fields
                .unnamed
                .iter()
                .enumerate()
                .map(|(pos, field)| FilterParameter {
                    name: Cow::Owned(Ident::new(&format!("arg_{}", pos), Span::call_site())),
                    is_optional: is_type_option(&field.ty),
                })
                .collect();
            let generate_constructor = quote! { ( #fields ) };

            (fields, generate_constructor)
        }

        Fields::Unit => {
            let fields = Punctuated::new();
            let generate_constructor = quote! {};

            (fields, generate_constructor)
        }
    };

    let construct_fields = fields
        .iter()
        .map(|field| generate_construct_field(&field.name, field.is_optional));

    let evaluate_fields = fields
        .iter()
        .map(|field| generate_evaluate_field(&field.name, field.is_optional));

    quote! {
        impl #name {
            fn new(mut args: liquid::compiler::FilterArguments) -> liquid::error::Result<Self> {
                #(#construct_fields)*

                args.check_args_exhausted()?;
                Ok( #name #generate_constructor )
            }

            fn evaluate<'a>(&'a self, context: &'a liquid::interpreter::Context) -> liquid::error::Result<#evaluated_name<'a>> {
                #(#evaluate_fields)*

                Ok( #evaluated_name #generate_constructor )
            }
        }
    }
}

/// Generates a field declaration for `EvaluatedFilterParameters` struct.
/// name: Expression -> name: &'a Value
/// name: Option<Expression> -> name: Option<&'a Value>
///
//possible error => liquid not a dependency (ex. liquid itself)
fn generate_evaluated_field(ident: Option<&Ident>, is_option: bool) -> TokenStream {
    // Should attrs from SliceParameters fields be transfered to EvaluatedSliceParameters fields?
    // Should vis from SliceParameters fields be transfered to EvaluatedSliceParameters fields?
    let ty = if is_option {
        quote! { Option<&'a liquid::value::Value> }
    } else {
        quote! { &'a liquid::value::Value }
    };

    if let Some(ident) = ident {
        quote! { #ident : #ty }
    } else {
        quote! { #ty }
    }
}

/// Generates `EvaluatedFilterParameters` struct with name `evaluated_name`, from input `structure`.
// Should evaluated_name be changeable by the user with an attribute?
// Should evaluated_name be "__EvaluatedSliceParameters" instead "EvaluatedSliceParameters" to avoid (already unlikely) name collisions?
fn generate_evaluated_struct(structure: &DeriveInput, evaluated_name: &Ident) -> TokenStream {
    // Should attrs from SliceParameters be transfered to EvaluatedSliceParameters?
    // Should vis from SliceParameters be transfered to EvaluatedSliceParameters?

    if let Data::Struct(data) = &structure.data {
        match &data.fields {
            Fields::Named(fields) => {
                let mut evaluated_fields: Punctuated<_, Token![,]> = Punctuated::new();
                for field in fields.named.iter() {
                    let ident = &field.ident;
                    evaluated_fields.push(generate_evaluated_field(
                        ident.as_ref(),
                        is_type_option(&field.ty),
                    ));
                }

                quote! {
                    struct #evaluated_name <'a>{
                        #evaluated_fields
                    }
                }
            }
            Fields::Unnamed(fields) => {
                let mut evaluated_fields: Punctuated<_, Token![,]> = Punctuated::new();
                for field in fields.unnamed.iter() {
                    let ident = &field.ident;
                    evaluated_fields.push(generate_evaluated_field(
                        ident.as_ref(),
                        is_type_option(&field.ty),
                    ));
                }

                quote! {
                    struct #evaluated_name <'a>(
                        #evaluated_fields
                    )
                }
            }
            Fields::Unit => {
                quote! {
                    struct #evaluated_name;
                }
            }
        }
    } else {
        panic!("generate_evaluated_struct must be used in a valid FilterParameters struct.");
    }
}

/// Helper function for `validate_filter_parameter_fields()`.
/// Given "liquid::interpreter::Expression", returns "Expression"
fn get_type_name<'a>(ty: &'a Type, msg: &str) -> Result<&'a PathSegment> {
    match ty {
        Type::Path(ty) => {
            // ty.qself : Is this relevant to validate?

            // what if `Expression` is not the actual name of the expected type? (ex. lack of `use`)
            // `Expression` could even be a whole different structure than expected
            let path = match ty.path.segments.last() {
                Some(path) => path.into_value(),
                None => return Err(Error::new_spanned(ty, msg)),
            };

            Ok(path)
        }
        ty => return Err(Error::new_spanned(ty, msg)),
    }
}

/// Validates a field inside the structure derived with `FilterParameters`
fn validate_filter_parameter_fields(fields: &Fields, msg: &str) -> Result<()> {
    for field in fields.iter() {
        let Field {
            attrs: _, // Attributes of the field : Is this relevant to validate?
            vis: _,   // Visibility of the field : Is this relevant to validate?
            ty,
            ..
        } = field;

        let path = get_type_name(ty, msg)?;
        match path.ident.to_string().as_str() {
            "Option" => match &path.arguments {
                PathArguments::AngleBracketed(arguments) => {
                    let args = &arguments.args;
                    if args.len() != 1 {
                        return Err(Error::new_spanned(ty, msg));
                    }
                    let arg = match args.last() {
                        Some(arg) => arg.into_value(),
                        None => return Err(Error::new_spanned(ty, msg)),
                    };

                    if let GenericArgument::Type(ty) = arg {
                        let path = get_type_name(ty, msg)?;
                        if path.ident.to_string().as_str() == "Expression" {
                            if path.arguments.is_empty() {
                                return Ok(());
                            }
                        }
                    }
                    return Err(Error::new_spanned(ty, msg));
                }
                _ => return Err(Error::new_spanned(ty, msg)),
            },
            "Expression" => {
                if !path.arguments.is_empty() {
                    return Err(Error::new_spanned(ty, msg));
                }
            }
            _ => return Err(Error::new_spanned(ty, msg)),
        };
    }
    Ok(())
}

/// Validates the structure derived with `FilterParameters`
fn validate_filter_parameter_struct(input: &DeriveInput) -> Result<()> {
    let DeriveInput {
        attrs: _, // Attributes of the struct : Is this relevant to validate?
        vis: _,   // Visibility of the struct : Is this relevant to validate?
        generics,
        data,
        ..
    } = input;

    if !generics.params.is_empty() {
        return Err(Error::new_spanned(
            generics,
            "Generics are cannot be used in FilterParameters.",
        ));
    }

    match data {
        Data::Struct(data) => {
            validate_filter_parameter_fields(
                &data.fields, 
                "Invalid type. All fields in FilterParameters must be either of type `Expression` or `Option<Expression>`"
            )
        }
        Data::Enum(data) => Err(Error::new_spanned(
            data.enum_token,
            "Enums cannot be FilterParameters.",
        )),
        Data::Union(data) => Err(Error::new_spanned(
            data.union_token,
            "Unions cannot be FilterParameters.",
        )),
    }
}

pub fn derive(input: &DeriveInput) -> TokenStream {
    if let Err(error) = validate_filter_parameter_struct(input) {
        return error.to_compile_error();
    }

    let name = &input.ident;
    let evaluated_name = Ident::new(&format!("Evaluated{}", name), Span::call_site());
    let fields = if let Data::Struct(data) = &input.data {
        &data.fields
    } else {
        panic!("Struct already validated.")
    };

    let mut output = TokenStream::new();
    output.extend(generate_impl_filter_parameters(
        name,
        &evaluated_name,
        fields,
    ));
    output.extend(generate_evaluated_struct(input, &evaluated_name));

    // Temporary
    // This println! shows the code that was generated by this macro when compiling
    println!("--------------------\n{}\n------------\n\n", output);

    output
}

