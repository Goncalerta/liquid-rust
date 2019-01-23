use proc_macro2::*;
use quote::*;
use std::borrow::Cow;
use syn::punctuated::Punctuated;
use syn::*;

struct FilterParser<'a> {
    name: &'a Ident,
    meta: FilterParserMeta,
}

struct FilterParserMeta {
    filter_name: String,
    filter_description: String,
    parameters_struct_name: Ident,
    filter_struct_name: Ident,
}

fn parse_filter_attribute(attr: &Attribute) -> Result<FilterParserMeta> {
    let meta = attr.parse_meta().map_err(|err| {
        Error::new(
            err.span(),
            format!("Could not parse `filter` attribute: {}", err),
        )
    })?;

    // TODO more modular parsing
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
                                meta, // TODO parenteses
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
                "FilterParserMeta does not declare `FilterParameters` struct. Have you tried `#[parser(name=\"...\", description=\"...\", parameters(...), parsed(...))]`?",
            ))?;
            let filter_struct_name = parsed.ok_or_else(|| Error::new_spanned(
                attr,
                "FilterParserMeta does not have a Filter to return. Have you tried `#[parser(name=\"...\", description=\"...\", parameters(...), parsed(...))]`?",
            ))?;
            

            Ok(FilterParserMeta {
                filter_name,
                filter_description,
                parameters_struct_name,
                filter_struct_name,
            })
        }
    }
}

// Maybe this can be obtained more reliably in Meta idents
fn is_filter_attribute(attr: &Attribute) -> bool {
    &attr
        .path
        .segments
        .last()
        .expect("An attribute has a name.")
        .into_value()
        .ident
        .to_string()
        == "filter"
}

/// Validates the structure derived with `FilterParserMeta`
fn validate_filter_parser_struct(input: &DeriveInput) -> Result<FilterParser> {
    let DeriveInput {
        attrs, data, ident, ..
    } = input;

    match data {
        Data::Struct(data) => match &data.fields {
            Fields::Named(fields) => {
                return Err(Error::new_spanned(
                    fields,
                    "FilterParserMeta may not have fields.",
                ))
            }
            Fields::Unnamed(fields) => {
                return Err(Error::new_spanned(
                    fields,
                    "FilterParserMeta may not have fields.",
                ))
            }
            Fields::Unit => {}
        },
        Data::Enum(data) => {
            return Err(Error::new_spanned(
                data.enum_token,
                "Enums cannot be FilterParsers.",
            ))
        }
        Data::Union(data) => {
            return Err(Error::new_spanned(
                data.union_token,
                "Unions cannot be FilterParsers.",
            ))
        }
    }

    for attr in attrs.iter() {
        if is_filter_attribute(attr) {
            return Ok(FilterParser {
                name: ident,
                meta: parse_filter_attribute(attr)?,
            });
        }
    }
    Err(Error::new_spanned(
        input,
        "FilterParserMeta does not have a `filter` attribute. Have you tried adding `#[parser(name=\"...\", description=\"...\", parameters(...), parsed(...))]`?",
    ))
}

fn generate_parse_filter(filter_parser: &FilterParser) -> TokenStream {
    let FilterParser {
        name: parser_name,
        meta:
            FilterParserMeta {
                parameters_struct_name,
                filter_struct_name,
                ..
            },
    } = filter_parser;

    quote! {
        impl ::liquid::compiler::ParseFilter for #parser_name {
            fn parse(&self, args: ::liquid::compiler::FilterArguments) -> Result<Box<::liquid::compiler::Filter>> {
                let args = #parameters_struct_name::new(args)?;
                Ok(Box::new(#filter_struct_name { args }))
            }
        }
    }
}

fn generate_reflection(filter_parser: &FilterParser) -> TokenStream {
    let FilterParser {
        name: parser_name,
        meta:
            FilterParserMeta {
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
                #parameters_struct_name::positional_parameters_reflection()
            }

            fn keyword_parameters(&self) -> &'static [::liquid::compiler::ParameterReflection] {
                 #parameters_struct_name::keyword_parameters_reflection()
            }
        }
    }
}

pub fn derive(input: &DeriveInput) -> TokenStream {
    let filter_parser = match validate_filter_parser_struct(input) {
        Ok(filter_parser) => filter_parser,
        Err(err) => return err.to_compile_error(),
    };

    // let name = &input.ident;
    // let evaluated_name = Ident::new(&format!("Evaluated{}", name), Span::call_site());
    // let fields = if let Data::Struct(data) = &input.data {
    //     &data.fields
    // } else {
    //     panic!("Struct already validated.")
    // };
    // let fields = match FilterParametersFields::new(fields) {
    //     Ok(fields) => fields,
    //     Err(error) => return error.to_compile_error(),
    // };

    let mut output = TokenStream::new();
    output.extend(generate_reflection(&filter_parser));
    output.extend(generate_parse_filter(&filter_parser));
    // output.extend(generate_impl_filter_parameters(
    //     name,
    //     &evaluated_name,
    //     &fields,
    // ));
    // output.extend(generate_reflection_helpers(name, &fields));
    // output.extend(generate_evaluated_struct(input, &evaluated_name));

    // Temporary
    // This println! shows the code that was generated by this macro when compiling
    println!("--------------------\n{}\n------------\n\n", output);

    output
}
