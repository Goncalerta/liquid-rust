use proc_macro2::*;
use proc_quote::*;
use std::borrow::Cow;
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::*;

// TODO should other attributes be taken into account (allowed, transfered to evaluated struct, ...)?

/// Struct that contains information to generate the necessary code for `FilterParameters`.
struct FilterParameters<'a> {
    name: &'a Ident,
    evaluated_name: Ident,
    fields: FilterParametersFields<'a>,
    vis: &'a Visibility
}

impl<'a> FilterParameters<'a> {
    /// Searches for `#[evaluated(...)]` in order to parse `evaluated_name`
    /// If attribute is not found, error message will use span in `input_span`
    fn parse_attrs(attrs: &Vec<Attribute>) -> Result<Option<Ident>> {
        let mut evaluated_attrs = attrs.iter().filter(|attr| attr.path.is_ident("evaluated"));

        match (evaluated_attrs.next(), evaluated_attrs.next()) {
            (Some(attr), None) => Ok(Some(Self::parse_evaluated_attr(attr)?)),

            (_, Some(attr)) => Err(Error::new_spanned(
                attr,
                "Found multiple definitions for `evaluated` attribute.",
            )),

            _ => Ok(None),
        }
    }

    fn parse_evaluated_attr(attr: &Attribute) -> Result<Ident> {
        let meta = attr.parse_meta().map_err(|err| {
            Error::new(
                err.span(),
                format!("Could not parse `evaluated` attribute: {}", err),
            )
        })?;

        match meta {
            Meta::Word(meta) => Err(Error::new_spanned(
                meta,
                "Couldn't parse evaluated attribute. Have you tried `#[evaluated(\"...\")]`?",
            )),
            Meta::NameValue(meta) => Err(Error::new_spanned(
                meta,
                "Couldn't parse evaluated attribute. Have you tried `#[evaluated(\"...\")]`?",
            )),
            Meta::List(meta) => {
                let meta_span = meta.span();
                let mut inner = meta.nested.into_iter();

                match (inner.next(), inner.next()) {
                    (Some(inner), None) => {
                        if let NestedMeta::Meta(Meta::Word(ident)) = inner {
                            Ok(ident)
                        } else {
                            Err(Error::new_spanned(inner, "Expected ident."))
                        }
                    }

                    (_, Some(inner)) => Err(Error::new_spanned(inner, "Unexpected element.")),

                    _ => Err(Error::new(meta_span, "Expected ident.")),
                }
            }
        }
    }

    /// Tries to create a new `FilterParameters` from the given `DeriveInput`
    fn from_input(input: &'a DeriveInput) -> Result<Self> {
        let DeriveInput {
            attrs,
            vis,
            generics,
            data,
            ident,
        } = input;

        if !generics.params.is_empty() {
            return Err(Error::new_spanned(
                generics,
                "Generics are cannot be used in FilterParameters.",
            ));
        }

        let fields = match data {
            Data::Struct(data) => FilterParametersFields::from_fields(&data.fields)?,
            Data::Enum(data) => {
                return Err(Error::new_spanned(
                    data.enum_token,
                    "Enums cannot be FilterParameters.",
                ));
            }
            Data::Union(data) => {
                return Err(Error::new_spanned(
                    data.union_token,
                    "Unions cannot be FilterParameters.",
                ));
            }
        };

        let name = ident;
        let evaluated_name = Self::parse_attrs(attrs)?
            .unwrap_or_else(|| Ident::new(&format!("Evaluated{}", name), Span::call_site()));

        Ok(FilterParameters {
            name,
            evaluated_name,
            fields,
            vis,
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
            // TODO what if Option and Expression mean different structures (because of `use`, or by having a path before)
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
                    return Err(Error::new_spanned(ty, Self::ERROR_INVALID_TYPE));
                } else {
                    return Ok(false);
                }
            }
            _ => return Err(Error::new_spanned(ty, Self::ERROR_INVALID_TYPE)),
        }
    }

    /// Creates a new `FilterParameter` from the given `field`, with the given `name`.
    fn new(name: Cow<'a, Ident>, field: &Field) -> Result<Self> {
        let is_optional = Self::parse_type_is_optional(&field.ty)?;
        let meta = FilterParameterMeta::from_field(&field)?;

        Ok(FilterParameter {
            name,
            is_optional,
            meta,
        })
    }

    /// Returns whether this field is optional.
    fn is_optional(&self) -> bool {
        self.is_optional
    }

    /// Returns whether this field is required (not optional).
    fn is_required(&self) -> bool {
        !self.is_optional
    }

    /// Returns whether this is a positional field.
    fn is_positional(&self) -> bool {
        self.meta.ty == FilterParameterType::Positional
    }

    /// Returns whether this is a keyword field.
    fn is_keyword(&self) -> bool {
        self.meta.ty == FilterParameterType::Keyword
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

/// Struct that contains information parsed in `#[parameter(...)]` attribute.
struct FilterParameterMeta {
    description: String,
    ty: FilterParameterType,
}

impl FilterParameterMeta {
    // TODO more modular parsing
    fn parse_parameter_attribute(attr: &Attribute) -> Result<Self> {
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
                // TODO don't allow multiple definitions
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

    /// Tries to create a new `FilterParserMeta` from the given field.
    fn from_field(field: &Field) -> Result<Self> {
        let mut parameter_attrs = field
            .attrs
            .iter()
            .filter(|attr| attr.path.is_ident("parameter"));

        match (parameter_attrs.next(), parameter_attrs.next()) {
            (Some(attr), None) => Self::parse_parameter_attribute(attr),

            (_, Some(attr)) => Err(Error::new_spanned(
                attr,
                "Found multiple definitions for `parameter` attribute.",
            )),

            _ => Err(Error::new_spanned(
                field,
                "Found parameter without #[parameter] attribute. All filter parameters must be accompanied by this attribute.",
            )),
        }
    }
}

/// Generates the statement that assigns the next positional argument in the iterator to `ident`.
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

/// Generates the match arm that assigns the given keyword argument.
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

/// Implements `FilterParameters`.
fn generate_impl_filter_parameters(filter_parameters: &FilterParameters) -> TokenStream {
    let FilterParameters {
        name,
        evaluated_name,
        fields,
        ..
    } = filter_parameters;

    let field_names = fields.parameters.iter().map(|field| &field.name);
    let generate_constructor = match &fields.ty {
        FilterParametersFieldsType::Named => quote! { { #(#field_names,)* } },
        FilterParametersFieldsType::Unnamed => quote! { ( #(#field_names,)* ) },
        FilterParametersFieldsType::Unit => quote! {},
    };

    let evaluate_fields = fields
        .parameters
        .iter()
        .map(|field| generate_evaluate_field(&field.name, field.is_optional()));

    let construct_positional_fields = fields
        .parameters
        .iter()
        .map(|field| generate_construct_positional_field(&field.name, field.is_optional()));

    let keyword_fields = fields
        .parameters
        .iter()
        .filter(|parameter| parameter.is_keyword());

    let match_keyword_parameters_arms = fields
        .parameters
        .iter()
        .filter(|parameter| parameter.is_keyword())
        .map(|field| generate_keyword_match_arm(&field.name));

    let required_keyword_fields = fields
        .parameters
        .iter()
        .filter(|parameter| parameter.is_keyword() && parameter.is_required());

    quote! {
        impl<'a> ::liquid::compiler::FilterParameters<'a> for #name {
            type EvaluatedFilterParameters = #evaluated_name<'a>;

            fn from_args(mut args: ::liquid::compiler::FilterArguments) -> ::liquid::error::Result<Self> {
                #(#construct_positional_fields)*
                if let Some(arg) = args.positional.next() {
                    return Err(::liquid::error::Error::with_msg("Too many positional parameters."));
                }

                #(let mut #keyword_fields = None;)*
                for arg in args.keyword {
                    match arg.0 {
                        #(#match_keyword_parameters_arms)*
                        keyword => return Err(::liquid::error::Error::with_msg(format!("Unexpected keyword parameter `{}`.", keyword))),
                    }
                }
                #(let #required_keyword_fields = #required_keyword_fields.ok_or_else(|| ::liquid::error::Error::with_msg("Required"))?;)*

                Ok( #name #generate_constructor )
            }

            fn evaluate(&'a self, context: &'a ::liquid::interpreter::Context) -> ::liquid::error::Result<Self::EvaluatedFilterParameters> {
               #(#evaluate_fields)*

                Ok( #evaluated_name #generate_constructor )
            }
        }
    }
}

/// Generates `EvaluatedFilterParameters` struct.
// TODO Should debug be added by default?
fn generate_evaluated_struct(filter_parameters: &FilterParameters) -> TokenStream {
    let FilterParameters { evaluated_name, fields, vis, .. } = filter_parameters;

    let field_types = fields.parameters.iter().map(|field| {
        if field.is_optional() {
            quote! { Option<&'a ::liquid::value::Value> }
        } else {
            quote! { &'a ::liquid::value::Value }
        }
    });

    match &fields.ty {
        FilterParametersFieldsType::Named => {
            let field_names = fields.parameters.iter().map(|field| &field.name);
            quote! {
                #[derive(Debug)]
                #vis struct #evaluated_name <'a>{
                    #(#field_names : #field_types,)*
                }
            }
        }
        FilterParametersFieldsType::Unnamed => {
            quote! {
                #[derive(Debug)]
                #vis struct #evaluated_name <'a>(
                    #(#field_types,)*
                )
            }
        }
        FilterParametersFieldsType::Unit => {
            quote! {
                #[derive(Debug)]
                #vis struct #evaluated_name;
            }
        }
    }
}

/// Constructs `ParameterReflection` for the given parameter.
fn generate_parameter_reflection(field: &FilterParameter) -> TokenStream {
    let name = &field.name.to_string();
    let description = &field.meta.description.to_string();
    let is_optional = field.is_optional();

    quote! {
        ParameterReflection {
            name: #name,
            description: #description,
            is_optional: #is_optional,
        },
    }
}

/// Implements `FilterParametersReflection`.
fn generate_reflection_helpers(filter_parameters: &FilterParameters) -> TokenStream {
    let FilterParameters { name, fields, .. } = filter_parameters;

    let kw_params_reflection = fields
        .parameters
        .iter()
        .filter(|parameter| parameter.is_keyword())
        .map(generate_parameter_reflection);

    let pos_params_reflection = fields
        .parameters
        .iter()
        .filter(|parameter| parameter.is_positional())
        .map(generate_parameter_reflection);

    quote! {
        impl ::liquid::compiler::FilterParametersReflection for #name {
            fn positional_parameters() -> &'static [::liquid::compiler::ParameterReflection] {
                &[ #(#pos_params_reflection)* ]
            }

            fn keyword_parameters() -> &'static [::liquid::compiler::ParameterReflection] {
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
    output.extend(generate_evaluated_struct(&filter_parameters));

    // Temporary TODO remove
    // This println! shows the code that was generated by this macro when compiling
    println!("--------------------\n{}\n------------\n\n", output);

    output
}
