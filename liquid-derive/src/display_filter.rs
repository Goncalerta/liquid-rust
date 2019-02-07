use proc_macro2::*;
use proc_quote::*;
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::*;
use helper_meta_parser::*;

// TODO cleanup

struct FilterStruct<'a> {
    name: &'a Ident,
    filter_name: String,
    parameters: Parameters<'a>,
    generics: &'a Generics,
}

enum Parameters<'a> {
    Ident(&'a Ident),
    Pos(usize),
}

impl<'a> Parameters<'a> {
    fn new(ident: Option<&'a Ident>, pos: usize) -> Self {
        match ident {
            Some(ident) => Parameters::Ident(ident),
            None => Parameters::Pos(pos),
        }
    }
}

impl<'a> ToTokens for Parameters<'a> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            Parameters::Ident(ident) => ident.to_tokens(tokens),
            Parameters::Pos(pos) => pos.to_tokens(tokens),
        }
    }
}

impl<'a> FilterStruct<'a> {

    /// Generates `impl` declaration of the given trait for the structure 
    /// represented by `self`.
    fn generate_impl(&self, trait_name: TokenStream) -> TokenStream {
        let name = &self.name;
        let (impl_generics, ty_generics, where_clause) = self.generics.split_for_impl();
        quote! {
            impl #impl_generics #trait_name for #name #ty_generics #where_clause
        }
    }


    /// Searches for `#[name(...)]` in order to parse `filter_name`.
    fn parse_attrs(attrs: &Vec<Attribute>) -> Result<String> {
        let mut evaluated_attrs = attrs.iter().filter(|attr| attr.path.is_ident("name"));

        match (evaluated_attrs.next(), evaluated_attrs.next()) {
            (Some(attr), None) => Self::parse_name_attr(attr),

            (_, Some(attr)) => Err(Error::new_spanned(
                attr,
                "Found multiple definitions for `name` attribute.",
            )),

            _ => Err(Error::new(
                Span::call_site(),
                "Cannot find `name` attribute in target struct. Have you tried adding `#[name = \"...\"]`?",
            )),
        }
    }

    /// Parses `#[name(...)]` attribute.
    fn parse_name_attr(attr: &Attribute) -> Result<String> {
        let meta = attr.parse_meta().map_err(|err| {
            Error::new(
                err.span(),
                format!("Could not parse `evaluated` attribute: {}", err),
            )
        })?;

        if let Meta::NameValue(meta) = meta {
            if let Lit::Str(name) = &meta.lit {
                Ok(name.value())
            } else {
                Err(Error::new_spanned(
                    &meta.lit,
                    "Expected string literal.",
                ))
            }
        } else {
            Err(Error::new_spanned(
                meta,
                "Couldn't parse evaluated attribute. Have you tried `#[evaluated(\"...\")]`?",
            ))
        }
    }

    fn from_input(input: &'a DeriveInput) -> Result<Self> {
        let DeriveInput { ident, generics, data, attrs, .. } = &input;
        let mut parameters = AssignOnce::Unset;
        
        let fields = match data {
            Data::Struct(data) => &data.fields,
            Data::Enum(data) => {
                return Err(Error::new_spanned(
                    data.enum_token,
                    "Filters cannot be `enum`s.",
                ));
            }
            Data::Union(data) => {
                return Err(Error::new_spanned(
                    data.union_token,
                    "Filters cannot be `union`s.",
                ));
            }
        };

        let fields_len = fields.iter().len();

        if fields_len == 0 {
            return Err(Error::new(
                Span::call_site(),
                "Target struct does not have any fields. A field to hold `FilterParameters` is required.",
            ));
        } else if fields_len == 1 {
            let field = fields.iter().next().expect("Guaranteed by if");
            let field = Parameters::new(field.ident.as_ref(), 0);
            parameters.set(field, ||()).expect("Guaranteed to be unset.");
        } else {
            let marked = fields
                .iter()
                .enumerate()
                .filter(|(_, field)| field.attrs.iter().any(|attr| attr.path.is_ident("parameters")));
            
            for (i, field) in marked {
                let params = Parameters::new(field.ident.as_ref(), i);
                parameters.set(params, || Error::new_spanned(
                    field,
                    "A previous field was already marked as `parameters`. Only one field can be marked as so.",
                ));
            } 
        }
        
        let name = ident;
        let filter_name = Self::parse_attrs(attrs)?;
        let parameters = parameters.unwrap_or_err(|| Error::new(
            Span::call_site(),
            "Cannot infer `FilterParameters` field in target struct. Mark this field with the `#[parameters]` attribute.",
        ))?;

        Ok(Self { name, filter_name, parameters, generics })
    }
}

pub fn derive(input: &DeriveInput) -> TokenStream {
    let filter = match FilterStruct::from_input(input) {
        Ok(filter) => filter,
        Err(err) => return err.to_compile_error(),
    };

    let FilterStruct { name, filter_name, parameters, .. } = &filter;

    let impl_display = filter.generate_impl(quote!{ ::std::fmt::Display });

    let output = quote! {
        #impl_display {
            fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
                ::std::write!(f, "{} : {}", #filter_name, &self.#parameters)
            }
        }
    };

    // Temporary TODO remove
    // This println! shows the code that was generated by this macro when compiling
    println!("--------------------\n{}\n------------\n\n", output);

    output
}
