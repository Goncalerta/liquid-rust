use proc_macro2::*;
use proc_quote::*;
use syn::*;

// TODO make parameters optional

/// Struct that contains information to generate the necessary code for `ParseFilter`.
struct ParseFilter<'a> {
    name: &'a Ident,
    meta: ParseFilterMeta,
}

impl<'a> ParseFilter<'a> {
    /// Asserts that this is an empty struct.
    fn validate_data(data: &Data) -> Result<()> {
        match data {
            Data::Struct(data) => match &data.fields {
                Fields::Named(fields) => Err(Error::new_spanned(
                    fields,
                    "ParseFilterMeta may not have fields.",
                )),
                Fields::Unnamed(fields) => Err(Error::new_spanned(
                    fields,
                    "ParseFilterMeta may not have fields.",
                )),
                Fields::Unit => Ok(()),
            },
            Data::Enum(data) => Err(Error::new_spanned(
                data.enum_token,
                "Enums cannot be ParseFilter.",
            )),
            Data::Union(data) => Err(Error::new_spanned(
                data.union_token,
                "Unions cannot be ParseFilter.",
            )),
        }
    }

    /// Searches for `#[filter(...)]` in order to parse `ParseFilterMeta`
    /// If attribute is not found, error message will use span in `input_span`
    fn parse_attrs(attrs: &Vec<Attribute>) -> Result<ParseFilterMeta> {
        let mut filter_attrs = attrs.iter().filter(|attr| attr.path.is_ident("filter"));

        match (filter_attrs.next(), filter_attrs.next()) {
            (Some(attr), None) => ParseFilterMeta::from_attr(attr),

            (_, Some(attr)) => Err(Error::new_spanned(
                attr,
                "Found multiple definitions for `filter` attribute.",
            )),

            _ => Err(Error::new(
                Span::call_site(),
                "Cannot find `filter` attribute in target struct. Have you tried adding `#[parser(name=\"...\", description=\"...\", parameters(...), parsed(...))]`?",
            )),
        }
    }

    /// Tries to create a new `ParseFilter` from the given `DeriveInput`
    fn from_input(input: &'a DeriveInput) -> Result<Self> {
        let DeriveInput {
            attrs, data, ident, ..
        } = input;

        Self::validate_data(&data)?;
        let meta = Self::parse_attrs(attrs)?;

        Ok(ParseFilter { name: ident, meta })
    }
}

/// Struct that contains information parsed in `#[filter(...)]` attribute.
struct ParseFilterMeta {
    filter_name: String,
    filter_description: String,
    parameters_struct_name: Ident,
    filter_struct_name: Ident,
}

impl ParseFilterMeta {
    /// Tries to create a new `ParseFilterMeta` from the given `DeriveInput`
    // TODO more modular parsing
    fn from_attr(attr: &Attribute) -> Result<Self> {
        let meta = attr.parse_meta().map_err(|err| {
            Error::new(
                err.span(),
                format!("Could not parse `filter` attribute: {}", err),
            )
        })?;

        match meta {
            Meta::Word(meta) => Err(Error::new_spanned(
                meta,
                "Found filter without name or description. Meta information is necessary in order to properly generate ParameterReflection.",
            )),
            Meta::NameValue(meta) => Err(Error::new_spanned(
                meta,
                "Couldn't parse this parameter attribute. Have you tried `#[parser(name=\"...\", description=\"...\", parameters(...), parsed(...))]`?",
            )),
            Meta::List(meta) => {
                // TODO don't allow multiple definitions
                let mut name = None;
                let mut description = None;
                let mut parameters = None;
                let mut parsed = None;

                for meta in meta.nested.into_iter() {
                    if let NestedMeta::Meta(meta) = meta {
                        if let Meta::NameValue(meta) = meta {
                            let key = &meta.ident.to_string();
                            let value = &meta.lit;

                            match key.as_str() {
                                "name" => {
                                    if let Lit::Str(value) = value {
                                        name = Some(value.value());
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
                                "parameters" => return Err(Error::new_spanned(
                                    key,
                                    "Did you mean `parameters(...)`.",
                                )),
                                _ => return Err(Error::new_spanned(
                                    key,
                                    "Unknown element in filter attribute.",
                                )),
                            }
                        } else if let Meta::List(meta) = meta {
                            let attr = &meta.ident;
                            let attr_name = attr.to_string();
                            
                            let mut meta = meta.nested.into_iter();
                            match (meta.next(), meta.next()) {
                                (Some(meta), None) => {
                                    if let NestedMeta::Meta(meta) = meta {
                                        if let Meta::Word(meta) = meta {
                                            match attr_name.as_str() {
                                                "parameters" => parameters = Some(meta),
                                                "parsed" => parsed = Some(meta),
                                                _ => return Err(Error::new_spanned(
                                                    attr,
                                                    "Unknown element in filter attribute.",
                                                )),
                                            }
                                            
                                        } else {
                                            return Err(Error::new_spanned(
                                                meta,
                                                "Unexpected element.",
                                            ))
                                        }
                                    } else {
                                        return Err(Error::new_spanned(
                                            meta,
                                            "Unexpected literal.",
                                        ))
                                    }
                                },
                                (_, Some(meta)) => return Err(Error::new_spanned(
                                    meta,
                                    "Unexpected element.",
                                )),
                                _ => return Err(Error::new_spanned(
                                    attr,
                                    "Element expected.",
                                )),

                            }
                        }else {
                            return Err(Error::new_spanned(
                                meta,
                                "Unknown element in filter attribute.",
                            ));
                        }
                    } else {
                        return Err(Error::new_spanned(
                            meta,
                            "Unknown element in filter attribute.",
                        ));
                    }
                }

                let filter_name = name.ok_or_else(|| Error::new_spanned(
                    attr,
                    "Filter does not have a name. Have you tried `#[parser(name=\"...\", description=\"...\", parameters(...), parsed(...))]`?",
                ))?;
                let filter_description = description.ok_or_else(|| Error::new_spanned(
                    attr,
                    "Filter does not have a description. Have you tried `#[parser(name=\"...\", description=\"...\", parameters(...), parsed(...))]`?",
                ))?;
                let parameters_struct_name = parameters.ok_or_else(|| Error::new_spanned(
                    attr,
                    "ParseFilterMeta does not declare `FilterParameters` struct. Have you tried `#[parser(name=\"...\", description=\"...\", parameters(...), parsed(...))]`?",
                ))?;
                let filter_struct_name = parsed.ok_or_else(|| Error::new_spanned(
                    attr,
                    "ParseFilterMeta does not have a Filter to return. Have you tried `#[parser(name=\"...\", description=\"...\", parameters(...), parsed(...))]`?",
                ))?;
                

                Ok(ParseFilterMeta {
                    filter_name,
                    filter_description,
                    parameters_struct_name,
                    filter_struct_name,
                })
            }
        }
    }
}

/// Generates implementation of `ParseFilter` for the given `ParseFilter`
fn generate_parse_filter(filter_parser: &ParseFilter) -> TokenStream {
    let ParseFilter {
        name: parser_name,
        meta:
            ParseFilterMeta {
                parameters_struct_name,
                filter_struct_name,
                ..
            },
    } = filter_parser;

    quote! {
        impl ::liquid::compiler::ParseFilter for #parser_name {
            fn parse(&self, args: ::liquid::compiler::FilterArguments) -> Result<Box<::liquid::compiler::Filter>> {
                let args = <#parameters_struct_name as ::liquid::compiler::FilterParameters>::from_args(args)?;
                let name = ::liquid::compiler::FilterReflection::name(self);
                Ok(Box::new(#filter_struct_name { args, name }))
            }
        }
    }
}

/// Generates implementation of `FilterReflection` for the given `ParseFilter`
fn generate_reflection(filter_parser: &ParseFilter) -> TokenStream {
    let ParseFilter {
        name: parser_name,
        meta:
            ParseFilterMeta {
                filter_name,
                filter_description,
                parameters_struct_name,
                ..
            },
    } = filter_parser;

    quote! {
        impl ::liquid::compiler::FilterReflection for #parser_name {
            fn name(&self) -> &'static str {
                #filter_name
            }
            fn description(&self) -> &'static str {
                #filter_description
            }

            fn positional_parameters(&self) -> &'static [::liquid::compiler::ParameterReflection] {
                <#parameters_struct_name as ::liquid::compiler::FilterParametersReflection>::positional_parameters()
            }

            fn keyword_parameters(&self) -> &'static [::liquid::compiler::ParameterReflection] {
                <#parameters_struct_name as ::liquid::compiler::FilterParametersReflection>::keyword_parameters()
            }
        }
    }
}

pub fn derive(input: &DeriveInput) -> TokenStream {
    let filter_parser = match ParseFilter::from_input(input) {
        Ok(filter_parser) => filter_parser,
        Err(err) => return err.to_compile_error(),
    };

    let mut output = TokenStream::new();
    output.extend(generate_reflection(&filter_parser));
    output.extend(generate_parse_filter(&filter_parser));

    // Temporary TODO remove
    // This println! shows the code that was generated by this macro when compiling
    println!("--------------------\n{}\n------------\n\n", output);

    output
}
