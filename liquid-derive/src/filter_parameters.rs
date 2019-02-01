use proc_macro2::*;
use proc_quote::*;
use std::borrow::Cow;
use syn::punctuated::Punctuated;
use syn::*;

// TODO should visibility in the original matter for the new structs?
// TODO should other attributes be taken into account (allowed, transfered to evaluated struct, ...)?
// TODO create trait for filter parameters and make filter_parser call using that trait
struct FilterParameters<'a> {
    name: &'a Ident,
    evaluated_name: Ident,
    fields: FilterParametersFields<'a>,
}

impl<'a> FilterParameters<'a> {
    fn from_input(input: &'a DeriveInput) -> Result<Self> {
        let DeriveInput {
            attrs, // Is this relevant to validate?
            vis: _,   // Is this relevant to validate?
            generics,
            data,
            ident,
            ..
        } = input;

        if !generics.params.is_empty() {
            return Err(Error::new_spanned(
                generics,
                "Generics are cannot be used in FilterParameters.",
            ));
        }

        let fields = match data {
            Data::Struct(data) => FilterParametersFields::from_fields(&data.fields)?,
            Data::Enum(data) => return Err(Error::new_spanned(
                data.enum_token,
                "Enums cannot be FilterParameters.",
            )),
            Data::Union(data) => return Err(Error::new_spanned(
                data.union_token,
                "Unions cannot be FilterParameters.",
            )),
        };

        let name = ident;
        // TODO Should evaluated_name be overridable with an attribute?
        let evaluated_name = Ident::new(&format!("Evaluated{}", name), Span::call_site()); 

        Ok(FilterParameters {
            name,
            evaluated_name,
            fields,
        })
    }
}

struct FilterParametersFields<'a> {
    ty: FilterParametersFieldsType,
    parameters: Punctuated<FilterParameter<'a>, Token![,]>,
}

impl<'a> FilterParametersFields<'a> {
    /// Tries to create a new `FilterParametersFields` from the given `Fields`
    fn from_fields(fields: &'a Fields) -> Result<Self> {
        match fields {
            Fields::Named(fields) => {
                let parameters = fields
                    .named
                    .iter()
                    .map(|field| {
                        let name = Cow::Borrowed(field.ident.as_ref().expect("Fields are named."));
                        FilterParameter::new(name, &field)
                    })
                    .collect::<Result<Punctuated<_, Token![,]>>>()?;

                let ty = FilterParametersFieldsType::Named;

                Ok(Self { parameters, ty })
            }

            Fields::Unnamed(fields) => {
                let parameters = fields
                    .unnamed
                    .iter()
                    .enumerate()
                    .map(|(pos, field)| {
                        let name =
                            Cow::Owned(Ident::new(&format!("arg_{}", pos), Span::call_site()));
                        FilterParameter::new(name, &field)
                    })
                    .collect::<Result<Punctuated<_, Token![,]>>>()?;

                let ty = FilterParametersFieldsType::Unnamed;

                Ok(Self { parameters, ty })
            }

            Fields::Unit => {
                let parameters = Punctuated::new();
                let ty = FilterParametersFieldsType::Unit;

                Ok(Self { parameters, ty })
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

enum FilterParametersFieldsType {
    Named,
    Unnamed,
    Unit,
}

struct FilterParameter<'a> {
    name: Cow<'a, Ident>,
    is_optional: bool,
    meta: FilterParameterMeta,
}

impl<'a> FilterParameter<'a> {
    /// This message is used a lot in other associated functions
    const ERROR_INVALID_TYPE: &'static str = "Invalid type. All fields in FilterParameters must be either of type `Expression` or `Option<Expression>`";

    /// Helper function for `validate_filter_parameter_fields()`.
    /// Given "::liquid::interpreter::Expression", returns "Expression"
    fn get_type_name(ty: &Type) -> Result<&PathSegment> {
        match ty {
            Type::Path(ty) => {
                // ty.qself : Is this relevant to validate?

                // what if `Expression` is not the actual name of the expected type? (ex. lack of `use`)
                // `Expression` could even be a whole different structure than expected
                let path = match ty.path.segments.last() {
                    Some(path) => path.into_value(),
                    None => return Err(Error::new_spanned(ty, Self::ERROR_INVALID_TYPE)),
                };

                Ok(path)
            }
            ty => return Err(Error::new_spanned(ty, Self::ERROR_INVALID_TYPE)),
        }
    }
    
    /// Returns Some(true) if type is optional, Some(false) if it's not and Err if not a valid type.
    /// 
    /// `Expression` => Some(false),
    /// `Option<Expression>` => Some(true),
    ///  _ => Err(...),
    fn parse_type_is_optional(ty: &Type) -> Result<bool> {
        let path = Self::get_type_name(ty)?;
        match path.ident.to_string().as_str() {
            "Option" => match &path.arguments {
                PathArguments::AngleBracketed(arguments) => {
                    let args = &arguments.args;
                    if args.len() != 1 {
                        return Err(Error::new_spanned(ty, Self::ERROR_INVALID_TYPE));
                    }
                    let arg = match args.last() {
                        Some(arg) => arg.into_value(),
                        None => return Err(Error::new_spanned(ty, Self::ERROR_INVALID_TYPE)),
                    };

                    if let GenericArgument::Type(ty) = arg {
                        let path = Self::get_type_name(ty)?;
                        if path.ident.to_string().as_str() == "Expression" {
                            if path.arguments.is_empty() {
                                return Ok(true);
                            }
                        }
                    }
                    return Err(Error::new_spanned(ty, Self::ERROR_INVALID_TYPE));
                }
                _ => return Err(Error::new_spanned(ty, Self::ERROR_INVALID_TYPE)),
            },
            "Expression" => {
                if !path.arguments.is_empty() {
                    return Err(Error::new_spanned(ty, Self::ERROR_INVALID_TYPE))
                } else {
                    return Ok(false)
                }
            }
            _ => return Err(Error::new_spanned(ty, Self::ERROR_INVALID_TYPE)),
        }
    }

    fn new(name: Cow<'a, Ident>, field: &Field) -> Result<Self> {
        let is_optional = Self::parse_type_is_optional(&field.ty)?;
        let meta = FilterParameterMeta::from_field(&field)?;

        Ok(FilterParameter {
            name,
            is_optional,
            meta,
        })
    }
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
    // evaluated_name: &'a str, // Should there be an attribute to rename evaluated filter parameters?
    // is_optional: bool, // Should there be an explicit required/optional attribute?
    description: String,
    ty: FilterParameterType,
}

impl FilterParameterMeta {
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

    fn from_field(field: &Field) -> Result<Self> {
        for attr in field.attrs.iter() {
            if Self::is_parameter_attribute(attr) {
                return Self::parse_parameter_attribute(attr);
            }
        }
        Err(Error::new_spanned(
            field,
            "Found parameter without #[parameter] attribute. All filter parameters must be accompanied by this attribute.",
        ))
    }
}

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

/// Generates `new()` and `evaluate()` methods for the struct named `name`
fn generate_impl_filter_parameters(filter_parameters: &FilterParameters) -> TokenStream {
    let FilterParameters {
        name,
        evaluated_name,
        fields,
    } = filter_parameters;

    let generate_constructor = fields.generate_constructor();

    let construct_fields = fields
        .parameters
        .iter()
        .map(|field| generate_construct_positional_field(&field.name, field.is_optional));

    let evaluate_fields = fields
        .parameters
        .iter()
        .map(|field| generate_evaluate_field(&field.name, field.is_optional));

    let keyword_parameters_idents = fields.keyword_parameters_idents();
    let match_keyword_parameters_arms = fields
        .keyword_parameters_idents()
        .map(generate_keyword_match_arm);
    let unwrap_required_keyword_fields = fields
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

fn generate_reflection_helpers(filter_parameters: &FilterParameters) -> TokenStream {
    let FilterParameters { name, fields, .. } = filter_parameters;

    let kw_params_reflection = fields
        .parameters
        .iter()
        .filter(|parameter| &parameter.meta.ty == &FilterParameterType::Keyword)
        .map(generate_parameter_reflection);

    let pos_params_reflection = fields
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





pub fn derive(input: &DeriveInput) -> TokenStream {
    let filter_parameters = match FilterParameters::from_input(input) {
        Ok(filter_parser) => filter_parser,
        Err(err) => return err.to_compile_error(),
    };

    let mut output = TokenStream::new();
    output.extend(generate_impl_filter_parameters(&filter_parameters));
    output.extend(generate_reflection_helpers(&filter_parameters));
    output.extend(generate_evaluated_struct(
        &input,
        &filter_parameters.evaluated_name,
    ));

    // Temporary TODO remove
    // This println! shows the code that was generated by this macro when compiling
    println!("--------------------\n{}\n------------\n\n", output);

    output
}

// TODO how to do unit tests with this?
// mod tests {
//     use super::*;

//     fn get_slice_filter_stream() -> TokenStream {
//         quote! {
//             #[derive(Debug,
//             //FilterParameters
//             )]
//             struct SliceParameters {
//                 offset: Expression,
//                 length: Option<Expression>,
//             }
//         }
//     }

//     #[test]
//     fn unit_test() {}
// }
