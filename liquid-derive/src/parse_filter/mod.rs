use helpers::*;
use proc_macro2::*;
use proc_quote::*;
use syn::*;

// TODO make FilterReflection a different derive macro

/// Struct that contains information to generate the necessary code for `ParseFilter`.
struct ParseFilter<'a> {
    name: &'a Ident,
    meta: ParseFilterMeta,
    generics: &'a Generics,
}

impl<'a> ParseFilter<'a> {

    /// Generates `impl` declaration of the given trait for the structure 
    /// represented by `self`.
    fn generate_impl(&self, trait_name: TokenStream) -> TokenStream {
        let name = &self.name;
        let (impl_generics, ty_generics, where_clause) = self.generics.split_for_impl();
        quote! {
            impl #impl_generics #trait_name for #name #ty_generics #where_clause
        }
    }

    /// Asserts that this is an empty struct.
    fn validate_data(data: &Data) -> Result<()> {
        match data {
            Data::Struct(data) => Ok(()),
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

    /// Searches for `#[filter(...)]` in order to parse `ParseFilterMeta`.
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
            attrs, data, ident, generics, ..
        } = input;

        Self::validate_data(&data)?;
        let meta = Self::parse_attrs(attrs)?;

        Ok(ParseFilter { name: ident, meta, generics })
    }
}

/// Struct that contains information parsed in `#[filter(...)]` attribute.
struct ParseFilterMeta {
    filter_name: String,
    filter_description: String,
    parameters_struct_name: Option<Ident>,
    filter_struct_name: Ident,
}

impl ParseFilterMeta {
    /// Tries to create a new `ParseFilterMeta` from the given `Attribute`
    fn from_attr(attr: &Attribute) -> Result<Self> {
        let meta = attr.parse_meta().map_err(|err| {
            Error::new(
                err.span(),
                format!("Could not parse `filter` attribute: {}", err),
            )
        })?;

        let meta = match meta {
            Meta::Word(meta) => return Err(Error::new_spanned(
                meta,
                "Found filter without name or description. Meta information is necessary in order to properly generate ParameterReflection.",
            )),
            Meta::NameValue(meta) => return Err(Error::new_spanned(
                meta,
                "Couldn't parse this parameter attribute. Have you tried `#[parser(name=\"...\", description=\"...\", parameters(...), parsed(...))]`?",
            )),
            Meta::List(meta) => meta,
        };

        let mut name = AssignOnce::Unset;
        let mut description = AssignOnce::Unset;
        let mut parameters = AssignOnce::Unset;
        let mut parsed = AssignOnce::Unset;

        for meta in meta.nested.into_iter() {
            match meta {
                NestedMeta::Meta(Meta::NameValue(meta)) => {
                    let key = &meta.ident;
                    let value = &meta.lit;

                    match key.to_string().as_str() {
                        "name" => assign_str_value(&mut name, key, value)?,
                        "description" => assign_str_value(&mut description, key, value)?,
                        "parameters" => {
                            Err(Error::new_spanned(key, "Did you mean `parameters(...)`."))?
                        }
                        _ => Err(Error::new_spanned(
                            key,
                            "Unknown element in filter attribute.",
                        ))?,
                    }
                }

                NestedMeta::Meta(Meta::List(meta)) => {
                    let attr = &meta.ident;

                    let mut meta = meta.nested.into_iter();
                    match (meta.next(), meta.next()) {
                        (Some(meta), None) => {
                            if let NestedMeta::Meta(Meta::Word(meta)) = meta {
                                match attr.to_string().as_str() {
                                    "parameters" => assign_ident(&mut parameters, attr, meta)?,
                                    "parsed" => assign_ident(&mut parsed, attr, meta)?,
                                    _ => {
                                        return Err(Error::new_spanned(
                                            attr,
                                            "Unknown element in filter attribute.",
                                        ));
                                    }
                                }
                            } else {
                                return Err(Error::new_spanned(
                                    meta,
                                    "Unexpected element in filter attribute.",
                                ));
                            }
                        }
                        (_, Some(meta)) => {
                            return Err(Error::new_spanned(
                                meta,
                                "Unexpected element in filter attribute.",
                            ));
                        }
                        _ => {
                            return Err(Error::new_spanned(
                                attr,
                                "Element expected in filter attribute.",
                            ));
                        }
                    }
                }

                _ => {
                    return Err(Error::new_spanned(
                        meta,
                        "Unknown element in filter attribute.",
                    ));
                }
            }
        }

        let filter_name = name.unwrap_or_err(|| Error::new_spanned(
            attr,
            "Filter does not have a name. Have you tried `#[parser(name=\"...\", description=\"...\", parameters(...), parsed(...))]`?",
        ))?;
        let filter_description = description.unwrap_or_err(|| Error::new_spanned(
            attr,
            "Filter does not have a description. Have you tried `#[parser(name=\"...\", description=\"...\", parameters(...), parsed(...))]`?",
        ))?;
        let parameters_struct_name = parameters.to_option();
        let filter_struct_name = parsed.unwrap_or_err(|| Error::new_spanned(
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

/// Generates implementation of `ParseFilter`.
fn generate_parse_filter(filter_parser: &ParseFilter) -> TokenStream {
    let ParseFilter {
        name: parser_name,
        meta:
            ParseFilterMeta {
                parameters_struct_name,
                filter_struct_name,
                ..
            },
        ..
    } = filter_parser;

    let impl_parse_filter = filter_parser.generate_impl(quote!{ ::liquid::compiler::ParseFilter });

    if let Some(parameters_struct_name) = parameters_struct_name {
        quote! {
            #impl_parse_filter {
                fn parse(&self, args: ::liquid::compiler::FilterArguments) -> Result<Box<::liquid::compiler::Filter>> {
                    let args = <#parameters_struct_name as ::liquid::compiler::FilterParameters>::from_args(args)?;
                    
                    Ok(Box::new(<#filter_struct_name as From<#parameters_struct_name>>::from(args)))
                }
            }
        }
    } else {
        quote! {
            #impl_parse_filter {
                fn parse(&self, mut args: ::liquid::compiler::FilterArguments) -> Result<Box<::liquid::compiler::Filter>> {
                    if let Some(arg) = args.positional.next() {
                        return Err(::liquid::error::Error::with_msg("Too many positional parameters."));
                    }
                    if let Some(arg) = args.keyword.next() {
                        return Err(::liquid::error::Error::with_msg(format!("Unexpected keyword parameter `{}`.", arg.0)));
                    }
                    
                    
                    Ok(Box::new(<#filter_struct_name as Default>::default()))
                }
            }
        }
    }
}

/// Generates implementation of `FilterReflection`.
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
        ..
    } = filter_parser;

    let impl_filter_reflection = filter_parser.generate_impl(quote!{ ::liquid::compiler::FilterReflection });

    let (positional_parameters, keyword_parameters) = if let Some(parameters_struct_name) =
        parameters_struct_name
    {
        (
            quote! { <#parameters_struct_name as ::liquid::compiler::FilterParametersReflection>::positional_parameters() },
            quote! { <#parameters_struct_name as ::liquid::compiler::FilterParametersReflection>::keyword_parameters() },
        )
    } else {
        (quote! { &[] }, quote! { &[] })
    };

    quote! {
        #impl_filter_reflection {
            fn name(&self) -> &'static str {
                #filter_name
            }
            fn description(&self) -> &'static str {
                #filter_description
            }

            fn positional_parameters(&self) -> &'static [::liquid::compiler::ParameterReflection] {
                #positional_parameters
            }

            fn keyword_parameters(&self) -> &'static [::liquid::compiler::ParameterReflection] {
                #keyword_parameters
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
