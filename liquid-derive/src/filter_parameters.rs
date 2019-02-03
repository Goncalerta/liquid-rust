use proc_macro2::*;
use proc_quote::*;
use std::borrow::Cow;
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::*;

// TODO #[parameter(default = "...")]
// TODO generate better liquid::errors.

/// Struct that contains information to generate the necessary code for `FilterParameters`.
struct FilterParameters<'a> {
    name: &'a Ident,
    evaluated_name: Ident,
    fields: FilterParametersFields<'a>,
    vis: &'a Visibility,
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

        if let Some(parameter) = fields.required_after_optional() {
            return Err(Error::new_spanned(
                parameter,
                "Found required positional parameter after an optional positional parameter. The user can't input this parameters without inputing the optional ones first.",
            ));
        }

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
    /// Returns the first required positional parameter (if any) that appears after an optional
    /// positional parameter.
    ///
    /// All optional positional parameters must appear after every required positional parameter.
    /// If this function returns `Some`, the macro is supposed to fail to compile.
    fn required_after_optional(&self) -> Option<&FilterParameter> {
        self.parameters
            .iter()
            .filter(|parameter| parameter.is_positional())
            .skip_while(|parameter| parameter.is_required())
            .skip_while(|parameter| parameter.is_optional())
            .next()
    }

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
        self.meta.mode == FilterParameterMode::Positional
    }

    /// Returns whether this is a keyword field.
    fn is_keyword(&self) -> bool {
        self.meta.mode == FilterParameterMode::Keyword
    }

    /// Returns the name of this parameter in liquid.
    ///
    /// That is, by default, the name of the field as a string. However,
    /// this name may be overriden by `rename` attribute.
    fn liquid_name(&self) -> String {
        match &self.meta.rename {
            Some(name) => name.clone(),
            None => self.name.to_string(),
        }
    }
}

impl<'a> ToTokens for FilterParameter<'a> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.name.to_tokens(tokens);
    }
}

#[derive(PartialEq)]
enum FilterParameterMode {
    Keyword,
    Positional,
}

enum FilterParameterType {
    // Any value, the default type
    Value,

    // Scalars
    Integer,
    Float,
    Bool,
    Date,
    Str,
}

/// Struct that contains information parsed in `#[parameter(...)]` attribute.
struct FilterParameterMeta {
    rename: Option<String>,
    description: String,
    mode: FilterParameterMode,
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
                let mut rename = None;
                let mut description = None;
                let mut mode = None;
                let mut ty = None;

                for meta in meta.nested.iter() {
                    if let NestedMeta::Meta(meta) = meta {
                        if let Meta::NameValue(meta) = meta {
                            let key = &meta.ident.to_string();
                            let value = &meta.lit;

                            match key.as_str() {
                                "rename" => {
                                    if let Lit::Str(value) = value {
                                        rename = Some(value.value());
                                    } else {
                                        return Err(Error::new_spanned(
                                            value,
                                            "Expected string literal.",
                                        ));
                                    }
                                },
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
                                        mode = match value.value().as_str() {
                                            "keyword" => Some(FilterParameterMode::Keyword),
                                            "positional" => Some(FilterParameterMode::Positional),
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
                                "value" => {
                                    if let Lit::Str(value) = value {
                                        ty = match value.value().as_str() {
                                            "any" => Some(FilterParameterType::Value),
                                            "whole number" => Some(FilterParameterType::Integer),
                                            "fractional number" => Some(FilterParameterType::Float),
                                            "boolean" => Some(FilterParameterType::Bool),
                                            "date" => Some(FilterParameterType::Date),
                                            "string" => Some(FilterParameterType::Str),
                                            _ => return Err(Error::new_spanned(
                                                value,
                                                "Expected one of the following: \"any\", \"whole number\", \"fractional number\", \"boolean\", \"date\" or \"string\".",
                                            )),
                                        };
                                    } else {
                                        return Err(Error::new_spanned(
                                            value,
                                            "Expected string literal.",
                                        ));
                                    }
                                },
                                "default" => {
                                    // TODO
                                    unimplemented!()
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
                let mode = mode.unwrap_or(FilterParameterMode::Positional);
                let ty = ty.unwrap_or(FilterParameterType::Value);

                Ok(FilterParameterMeta {
                    rename,
                    description,
                    mode,
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

/// Generates the statement that assigns the next positional argument.
fn generate_construct_positional_field(field: &FilterParameter) -> TokenStream {
    let name = &field.name;

    if field.is_optional() {
        quote! {
            let #name = args.positional.next();
        }
    } else {
        quote! {
            let #name = args.positional.next().ok_or_else(|| ::liquid::error::Error::with_msg("Required"))?;
        }
    }
}

/// Generates the statement that evaluates the `Expression`
fn generate_evaluate_field(field: &FilterParameter) -> TokenStream {
    let name = &field.name;
    let ty = &field.meta.ty;

    let to_type = match ty {
        FilterParameterType::Value => quote! {},
        FilterParameterType::Integer => quote! {
            .as_scalar()
            .and_then(::liquid::value::Scalar::to_integer)
            .ok_or_else(||
                ::liquid::error::Error::with_msg("Invalid argument")
                    .context("cause", "Whole number expected"))?
        },
        FilterParameterType::Float => quote! {
            .as_scalar()
            .and_then(::liquid::value::Scalar::to_float)
            .ok_or_else(||
                ::liquid::error::Error::with_msg("Invalid argument")
                    .context("cause", "Fractional number expected"))?
        },
        FilterParameterType::Bool => quote! {
            .as_scalar()
            .and_then(::liquid::value::Scalar::to_bool)
            .ok_or_else(||
                ::liquid::error::Error::with_msg("Invalid argument")
                    .context("cause", "Boolean expected"))?
        },
        FilterParameterType::Date => quote! {
            .as_scalar()
            .and_then(::liquid::value::Scalar::to_date)
            .ok_or_else(||
                ::liquid::error::Error::with_msg("Invalid argument")
                    .context("cause", "Date expected"))?
        },
        FilterParameterType::Str => quote! {
            .as_scalar()
            .map(::liquid::value::Scalar::to_str)
            .ok_or_else(||
                ::liquid::error::Error::with_msg("Invalid argument")
                    .context("cause", "String expected"))?
        },
    };

    if field.is_optional() {
        quote! {
            let #name = match &self.#name {
                Some(field) => Some(field.evaluate(context)? #to_type),
                None => None,
            };
        }
    } else {
        quote! {
            let #name = self.#name.evaluate(context)? #to_type ;
        }
    }
}

/// Generates the match arm that assigns the given keyword argument.
fn generate_keyword_match_arm(field: &FilterParameter) -> TokenStream {
    let rust_name = &field.name;
    let liquid_name = field.liquid_name();

    quote! {
        #liquid_name => if #rust_name.is_none() {
            #rust_name = Some(arg.1);
        } else {
            return Err(::liquid::error::Error::with_msg(concat!("Multiple definitions of ", #liquid_name, ".")));
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
    let comma_separated_field_names = quote! { #(#field_names,)* };
    let generate_constructor = match &fields.ty {
        FilterParametersFieldsType::Named => quote! { { #comma_separated_field_names } },
        FilterParametersFieldsType::Unnamed => quote! { ( #comma_separated_field_names ) },
        FilterParametersFieldsType::Unit => quote! {},
    };

    let generate_evaluated_constructor = match &fields.ty {
        FilterParametersFieldsType::Named => {
            quote! { { #comma_separated_field_names __phantom_data: ::std::marker::PhantomData } }
        }
        FilterParametersFieldsType::Unnamed => {
            quote! { ( #comma_separated_field_names __phantom_data: ::std::marker::PhantomData ) }
        }
        FilterParametersFieldsType::Unit => {
            quote! { { __phantom_data: ::std::marker::PhantomData } }
        }
    };

    let evaluate_fields = fields
        .parameters
        .iter()
        .map(|field| generate_evaluate_field(&field));

    let construct_positional_fields = fields
        .parameters
        .iter()
        .filter(|parameter| parameter.is_positional())
        .map(|field| generate_construct_positional_field(&field));

    let keyword_fields = fields
        .parameters
        .iter()
        .filter(|parameter| parameter.is_keyword());

    let match_keyword_parameters_arms = fields
        .parameters
        .iter()
        .filter(|parameter| parameter.is_keyword())
        .map(|field| generate_keyword_match_arm(&field));

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

                Ok( #evaluated_name #generate_evaluated_constructor )
            }
        }
    }
}

/// Generates `EvaluatedFilterParameters` struct.
fn generate_evaluated_struct(filter_parameters: &FilterParameters) -> TokenStream {
    let FilterParameters {
        evaluated_name,
        fields,
        vis,
        ..
    } = filter_parameters;

    let field_types = fields.parameters.iter().map(|field| {
        let ty = match &field.meta.ty {
            FilterParameterType::Value => quote! {&'a ::liquid::value::Value},
            FilterParameterType::Integer => quote! { i32 },
            FilterParameterType::Float => quote! { f64 },
            FilterParameterType::Bool => quote! { bool },
            FilterParameterType::Date => quote! { ::liquid::value::Date },
            FilterParameterType::Str => quote! { ::std::borrow::Cow<'a, str> },
        };

        if field.is_optional() {
            quote! { Option< #ty > }
        } else {
            quote! { #ty }
        }
    });

    match &fields.ty {
        FilterParametersFieldsType::Named => {
            let field_names = fields.parameters.iter().map(|field| &field.name);
            quote! {
                #vis struct #evaluated_name <'a>{
                    #(#field_names : #field_types,)*
                    __phantom_data: ::std::marker::PhantomData<&'a ()>
                }
            }
        }
        FilterParametersFieldsType::Unnamed => {
            quote! {
                #vis struct #evaluated_name <'a>(
                    #(#field_types,)*
                    __phantom_data: ::std::marker::PhantomData<&'a ()>
                )
            }
        }
        FilterParametersFieldsType::Unit => {
            quote! {
                #vis struct #evaluated_name <'a> {
                    __phantom_data: ::std::marker::PhantomData<&'a ()>
                }
            }
        }
    }
}

/// Constructs `ParameterReflection` for the given parameter.
fn generate_parameter_reflection(field: &FilterParameter) -> TokenStream {
    let name = field.liquid_name();
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
fn generate_impl_reflection(filter_parameters: &FilterParameters) -> TokenStream {
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

/// Helper function for `generate_impl_display`
fn generate_access_positional_field_for_display(field: &FilterParameter) -> TokenStream {
    let rust_name = &field.name;

    if field.is_optional() {
        quote! {
            self.#rust_name.as_ref()
        }
    } else {
        quote! {
            Some(&self.#rust_name)
        }
    }
}

/// Helper function for `generate_impl_display`
fn generate_access_keyword_field_for_display(field: &FilterParameter) -> TokenStream {
    let rust_name = &field.name;
    let liquid_name = field.liquid_name();

    if field.is_optional() {
        quote! {
            (#liquid_name, self.#rust_name.as_ref())
        }
    } else {
        quote! {
            (#liquid_name, Some(&self.#rust_name))
        }
    }
}

/// Implements `Display`
fn generate_impl_display(filter_parameters: &FilterParameters) -> TokenStream {
    let FilterParameters { name, fields, .. } = filter_parameters;

    let positional_fields = fields
        .parameters
        .iter()
        .filter(|parameter| parameter.is_positional())
        .map(|field| generate_access_positional_field_for_display(&field));

    let keyword_fields = fields
        .parameters
        .iter()
        .filter(|parameter| parameter.is_keyword())
        .map(|field| generate_access_keyword_field_for_display(&field));

    quote! {
        impl ::std::fmt::Display for #name {
            fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
                let positional = [#(#positional_fields ,)*];
                let keyword = [#(#keyword_fields ,)*];

                let positional = positional
                    .iter()
                    .filter_map(|p| p.as_ref())
                    .map(|p| p.to_string());
                let keyword = keyword.iter().filter_map(|p| match p.1 {
                    Some(p1) => Some(format!("{}: {}", p.0, p1)),
                    None => None,
                });

                let parameters = positional
                    .chain(keyword)
                    .collect::<Vec<String>>()
                    .join(", ");

                write!(
                    f,
                    "{}",
                    parameters
                )
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
    output.extend(generate_impl_reflection(&filter_parameters));
    output.extend(generate_impl_display(&filter_parameters));
    output.extend(generate_evaluated_struct(&filter_parameters));

    // Temporary TODO remove
    // This println! shows the code that was generated by this macro when compiling
    println!("--------------------\n{}\n------------\n\n", output);

    output
}
