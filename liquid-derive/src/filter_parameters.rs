use proc_macro2::*;
use quote::*;
use std::borrow::Cow;
use syn::punctuated::Punctuated;
use syn::*;

/// Generates the statement that saves the next positional argument in the iterator in `ident`.
fn generate_construct_positional_field(ident: &Ident, is_option: bool) -> TokenStream {
    if is_option {
        quote! {
            let #ident = args.positional.next();
        }
    } else {
        quote! {
            let #ident = args.positional.next().ok_or_else(|| ::liquid::error::Error::with_msg("Required"))?;
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
            let #ident = &self.#ident.evaluate(context)?;
        }
    }
}

fn generate_keyword_match_arm(keyword: &Ident) -> TokenStream {
    let keyword_string = keyword.to_string();
    quote! {
        #keyword_string => if #keyword.is_none() {
            #keyword = Some(arg.1);
        } else {
            return Err(::liquid::error::Error::with_msg(concat!("Multiple definitions of ", #keyword_string, ".")));
        },
    }
}

fn generate_unwrap_keyword_field(ident: &Ident) -> TokenStream {
    quote! {
        let #ident = #ident.ok_or_else(|| ::liquid::error::Error::with_msg("Required"))?;
    }
}

/// Checks whether this type is `Option<Expression>` (true) or `Expression` (false)
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

enum FilterParametersFieldsType {
    Named,
    Unnamed,
    Unit,
}

struct FilterParametersFields<'a> {
    ty: FilterParametersFieldsType,
    parameters: Punctuated<FilterParameter<'a>, Token![,]>,
}
impl<'a> FilterParametersFields<'a> {
    fn new(fields: &'a Fields) -> Result<Self> {
        match fields {
            Fields::Named(fields) => {
                let parameters = fields
                    .named
                    .iter()
                    .map(|field| {
                        Ok(FilterParameter {
                            name: Cow::Borrowed(&field.ident.as_ref().expect("Fields are named.")),
                            is_optional: is_type_option(&field.ty),
                            meta: FilterParameterMeta::new(&field)?,
                        })
                    })
                    .collect::<Result<Punctuated<_, Token![,]>>>()?;

                Ok(Self {
                    parameters,
                    ty: FilterParametersFieldsType::Named,
                })
            }

            Fields::Unnamed(fields) => {
                let parameters = fields
                    .unnamed
                    .iter()
                    .enumerate()
                    .map(|(pos, field)| {
                        Ok(FilterParameter {
                            // TODO If there is no name keyword attribute should not be valid
                            name: Cow::Owned(Ident::new(
                                &format!("arg_{}", pos),
                                Span::call_site(),
                            )),
                            is_optional: is_type_option(&field.ty),
                            meta: FilterParameterMeta::new(&field)?,
                        })
                    })
                    .collect::<Result<Punctuated<_, Token![,]>>>()?;

                Ok(Self {
                    parameters,
                    ty: FilterParametersFieldsType::Unnamed,
                })
            }

            Fields::Unit => {
                let parameters = Punctuated::new();

                Ok(Self {
                    parameters,
                    ty: FilterParametersFieldsType::Unit,
                })
            }
        }
    }

    fn generate_constructor(&self) -> TokenStream {
        let fields = &self.parameters;
        match &self.ty {
            FilterParametersFieldsType::Named => quote! { { #fields } },
            FilterParametersFieldsType::Unnamed => quote! { ( #fields ) },
            FilterParametersFieldsType::Unit => quote! {},
        }
    }

    fn keyword_parameters_idents(&'a self) -> Box<Iterator<Item = &Ident> + 'a> {
        Box::new(
            self.parameters
                .iter()
                .filter(|parameter| &parameter.meta.ty == &FilterParameterType::Keyword)
                .map(|parameter| &parameter.name as &Ident),
        )
    }

    fn required_keyword_parameters_idents(&'a self) -> Box<Iterator<Item = &Ident> + 'a> {
        Box::new(
            self.parameters
                .iter()
                .filter(|parameter| {
                    &parameter.meta.ty == &FilterParameterType::Keyword && !&parameter.is_optional
                })
                .map(|parameter| &parameter.name as &Ident),
        )
    }
}

/// Helper struct for `generate_impl_filter_parameters`
struct FilterParameter<'a> {
    name: Cow<'a, Ident>,
    is_optional: bool,
    meta: FilterParameterMeta,
}
impl<'a> ToTokens for FilterParameter<'a> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.name.to_tokens(tokens);
    }
}

#[derive(PartialEq)]
enum FilterParameterType {
    Keyword,
    Positional,
}

struct FilterParameterMeta {
    // name: &'a str, // Should there be a rename attribute?
    // is_optional: bool, // Should there be an explicit required/optional attribute?
    description: String,
    ty: FilterParameterType,
}

impl FilterParameterMeta {
    fn new(field: &Field) -> Result<Self> {
        for attr in field.attrs.iter() {
            if is_parameter_attribute(attr) {
                return parse_parameter_attribute(attr);
            }
        }
        Err(Error::new_spanned(
            field,
            "Found parameter without #[parameter] attribute. All filter parameters must be accompanied by this attribute.",
        ))
    }
}

/// Generates `new()` and `evaluate()` methods for the struct named `name`
fn generate_impl_filter_parameters(
    name: &Ident,
    evaluated_name: &Ident,
    parameters: &FilterParametersFields,
) -> TokenStream {
    let generate_constructor = parameters.generate_constructor();

    let construct_fields = parameters
        .parameters
        .iter()
        .map(|field| generate_construct_positional_field(&field.name, field.is_optional));

    let evaluate_fields = parameters
        .parameters
        .iter()
        .map(|field| generate_evaluate_field(&field.name, field.is_optional));

    let keyword_parameters_idents = parameters.keyword_parameters_idents();
    let match_keyword_parameters_arms = parameters
        .keyword_parameters_idents()
        .map(generate_keyword_match_arm);
    let unwrap_required_keyword_fields = parameters
        .required_keyword_parameters_idents()
        .map(generate_unwrap_keyword_field);

    quote! {
        impl #name {
            fn new(mut args: ::liquid::compiler::FilterArguments) -> ::liquid::error::Result<Self> {
                #(#construct_fields)*
                if let Some(arg) = args.positional.next() {
                    return Err(::liquid::error::Error::with_msg("Too many positional parameters."));
                }

                #(let mut #keyword_parameters_idents = None;)*
                for arg in args.keyword {
                    match arg.0 {
                        #(#match_keyword_parameters_arms)*
                        keyword => return Err(::liquid::error::Error::with_msg(format!("Unexpected keyword parameter `{}`.", keyword))),
                    }
                }
                #(#unwrap_required_keyword_fields)*

                Ok( #name #generate_constructor )
            }

            fn evaluate<'a>(&'a self, context: &'a ::liquid::interpreter::Context) -> ::liquid::error::Result<#evaluated_name<'a>> {
               #(#evaluate_fields)*

                Ok( #evaluated_name #generate_constructor )
            }
        }
    }
}

/// Generates a field declaration for `EvaluatedFilterParameters` struct.
/// name: Expression -> name: &'a Value
/// name: Option<Expression> -> name: Option<&'a Value>
fn generate_evaluated_field(ident: Option<&Ident>, is_option: bool) -> TokenStream {
    // Should attrs from SliceParameters fields be transfered to EvaluatedSliceParameters fields?
    // Should vis from SliceParameters fields be transfered to EvaluatedSliceParameters fields?
    let ty = if is_option {
        quote! { Option<&'a ::liquid::value::Value> }
    } else {
        quote! { &'a ::liquid::value::Value }
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
                    #[derive(Debug)]
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
                    #[derive(Debug)]
                    struct #evaluated_name <'a>(
                        #evaluated_fields
                    )
                }
            }
            Fields::Unit => {
                quote! {
                    #[derive(Debug)]
                    struct #evaluated_name;
                }
            }
        }
    } else {
        panic!("generate_evaluated_struct must be used in a valid FilterParameters struct.");
    }
}

fn generate_parameter_reflection(field: &FilterParameter) -> TokenStream {
    let name = &field.name.to_string();
    let description = &field.meta.description.to_string();
    let is_optional = &field.is_optional;

    quote! {
        ParameterReflection {
            name: #name,
            description: #description,
            is_optional: #is_optional,
        },
    }
}

fn generate_reflection_helpers(name: &Ident, parameters: &FilterParametersFields) -> TokenStream {
    let kw_params_reflection = parameters
        .parameters
        .iter()
        .filter(|parameter| &parameter.meta.ty == &FilterParameterType::Keyword)
        .map(generate_parameter_reflection);

    let pos_params_reflection = parameters
        .parameters
        .iter()
        .filter(|parameter| &parameter.meta.ty == &FilterParameterType::Positional)
        .map(generate_parameter_reflection);

    quote! {
        impl #name {
            fn positional_parameters_reflection() -> &'static [::liquid::compiler::ParameterReflection] {
                &[ #(#pos_params_reflection)* ]
            }

            fn keyword_parameters_reflection() -> &'static [::liquid::compiler::ParameterReflection] {
                &[ #(#kw_params_reflection)* ]
            }
        }
    }
}

// Maybe this can be obtained more reliably in Meta idents
fn is_parameter_attribute(attr: &Attribute) -> bool {
    &attr
        .path
        .segments
        .last()
        .expect("An attribute has a name.")
        .into_value()
        .ident
        .to_string()
        == "parameter"
}

fn parse_parameter_attribute(attr: &Attribute) -> Result<FilterParameterMeta> {
    let meta = attr.parse_meta().map_err(|err| {
        Error::new(
            err.span(),
            format!("Could not parse `parameter` attribute: {}", err),
        )
    })?;

    match meta {
        Meta::Word(meta) => Err(Error::new_spanned(
            meta,
            "Found parameter without description. Description is necessary in order to properly generate ParameterReflection.",
        )),
        Meta::NameValue(meta) => Err(Error::new_spanned(
            meta,
            "Couldn't parse this parameter attribute. Have you tried `#[parameter(description=\"...\")]`?",
        )),
        Meta::List(meta) => {
            let mut description = None;
            let mut ty = None;

            for meta in meta.nested.iter() {
                if let NestedMeta::Meta(meta) = meta {
                    if let Meta::NameValue(meta) = meta {
                        let key = &meta.ident.to_string();
                        let value = &meta.lit;

                        match key.as_str() {
                            "description" => {
                                if let Lit::Str(value) = value {
                                    description = Some(value.value());
                                } else {
                                    return Err(Error::new_spanned(
                                        value,
                                        "Expected string literal.",
                                    ));
                                }
                            },
                            "mode" => {
                                if let Lit::Str(value) = value {
                                    ty = match value.value().as_str() {
                                        "keyword" => Some(FilterParameterType::Keyword),
                                        "positional" => Some(FilterParameterType::Positional),
                                        _ => return Err(Error::new_spanned(
                                            value,
                                            "Expected either \"keyword\" or \"positional\".",
                                        )),
                                    };
                                } else {
                                    return Err(Error::new_spanned(
                                        value,
                                        "Expected string literal.",
                                    ));
                                }
                            },
                            _ => return Err(Error::new_spanned(
                                key,
                                "Unknown element in parameter attribute.",
                            )),
                        }
                    } else {
                        return Err(Error::new_spanned(
                            meta,
                            "Unknown element in parameter attribute. All elements should be key=value pairs.",
                        ));
                    }
                } else {
                    return Err(Error::new_spanned(
                        meta,
                        "Unknown element in parameter attribute. All elements should be key=value pairs.",
                    ));
                }
            }

            let description = description.ok_or_else(|| Error::new_spanned(
                meta,
                "Found parameter without description. Description is necessary in order to properly generate ParameterReflection.",
            ))?;
            let ty = ty.unwrap_or(FilterParameterType::Positional);

            Ok(FilterParameterMeta {
                description,
                ty,
            })
        }
    }
}

/// Helper function for `validate_filter_parameter_fields()`.
/// Given "::liquid::interpreter::Expression", returns "Expression"
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
            attrs,  // Attributes of the field : Is this relevant to validate?
            vis: _, // Visibility of the field : Is this relevant to validate?
            ty,
            ..
        } = field;

        // TODO Meta must not be lost
        for attr in attrs {
            parse_parameter_attribute(attr);
        }

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
        Data::Struct(data) => validate_filter_parameter_fields(
            &data.fields, 
            "Invalid type. All fields in FilterParameters must be either of type `Expression` or `Option<Expression>`"
        ),
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
    let fields = match FilterParametersFields::new(fields) {
        Ok(fields) => fields,
        Err(error) => return error.to_compile_error(),
    };

    let mut output = TokenStream::new();
    output.extend(generate_impl_filter_parameters(
        name,
        &evaluated_name,
        &fields,
    ));
    output.extend(generate_reflection_helpers(name, &fields));
    output.extend(generate_evaluated_struct(input, &evaluated_name));

    // Temporary
    // This println! shows the code that was generated by this macro when compiling
    println!("--------------------\n{}\n------------\n\n", output);

    output
}

