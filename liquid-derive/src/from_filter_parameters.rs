use proc_macro2::*;
use proc_quote::*;
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::*;
use helper_meta_parser::*;

struct FilterStruct<'a> {
    name: &'a Ident,
    parameters_struct_name: &'a Type,
    fields: Vec<FilterStructField<'a>>
}

impl<'a> FilterStruct<'a> {
    fn from_input(input: &'a DeriveInput) -> Result<Self> {
        let DeriveInput { ident, generics, data, .. } = &input;
        let mut parameters_struct_name = AssignOnce::Unset;
        let mut filter_fields = Vec::new();
        
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
        let mut fields = fields.iter();

        if fields_len == 0 {
            return Err(Error::new(
                Span::call_site(),
                "Target struct does not have any fields. A field to hold `FilterParameters` is required.",
            ));
        } else if fields_len == 1 {
            let field = fields.next().expect("Guaranteed by if");
            parameters_struct_name.set(&field.ty, ||()).expect("Guaranteed to be unset.");
            let field = FilterStructField::new(field.ident.as_ref(), &field.ty, true);
            filter_fields.push(field)
        } else {
            for field in fields {
                let filter_field = FilterStructField::from_field(field);
                if filter_field.is_filter_parameters() {
                    parameters_struct_name.set(&field.ty, || Error::new_spanned(
                        field,
                        "A previous field was already marked as `parameters`. Only one field can be marked as so.",
                    ))?;
                }
                filter_fields.push(filter_field);
            } 
        }

        

        let name = ident;
        let fields = filter_fields;
        let parameters_struct_name = parameters_struct_name.unwrap_or_err(|| Error::new(
            Span::call_site(),
            "Cannot infer `FilterParameters` field in target struct. Mark this field with the `#[parameters]` attribute.",
        ))?;

        Ok(Self { name, fields, parameters_struct_name })
    }
}

enum FilterStructField<'a> {
    FilterParameters(FilterField<'a>),
    RegularField(FilterField<'a>)
}

impl<'a> FilterStructField<'a> {
    fn new(ident: Option<&'a Ident>, ty: &'a Type, is_filter_parameters: bool) -> Self {
        let field = FilterField { ident, ty };
        if is_filter_parameters {
            FilterStructField::FilterParameters(field)
        } else {
            FilterStructField::RegularField(field)
        } 
    }

    fn from_field(field: &'a Field) -> Self {
        let Field { attrs, ident, ty, .. } = field;

        let ident = ident.as_ref();

        if attrs.iter().any(|attr| attr.path.is_ident("parameters") && attr.tts.is_empty()) {
            FilterStructField::FilterParameters(FilterField { ident, ty })
        } else {
            FilterStructField::RegularField(FilterField { ident, ty })
        }
    }

    fn generate_field_value(&self) -> TokenStream {
        match self {
            FilterStructField::FilterParameters(field) => {
                let FilterField { ident, .. } = field;
                if let Some(ident) = ident {
                    quote! {
                        #ident: parameters,
                    }
                } else {
                    quote! {
                        parameters,
                    }
                }
            },
            FilterStructField::RegularField(field) => {
                let FilterField { ident, ty } = field;
                if let Some(ident) = ident {
                    quote! {
                        #ident: <#ty as Default>::default()
                    }
                } else {
                    quote! {
                        <#ty as Default>::default()
                    }
                }
                
            },
        }
    }

    fn is_filter_parameters(&self) -> bool {
        match self {
            FilterStructField::FilterParameters(_) => true,
            _ => false,
        }
    }
}

struct FilterField<'a> {
    ident: Option<&'a Ident>,
    ty: &'a Type
}




pub fn derive(input: &DeriveInput) -> TokenStream {
    let filter = match FilterStruct::from_input(input) {
        Ok(filter) => filter,
        Err(err) => return err.to_compile_error(),
    };

    let FilterStruct { name, parameters_struct_name, fields } = filter;
    let fields = fields.iter().map(|field| field.generate_field_value());


    let output = quote! {
        impl From<#parameters_struct_name> for #name {
            fn from(parameters: #parameters_struct_name) -> Self {
                Self { 
                    #(#fields)*
                }
            }
        }
    };

    // Temporary TODO remove
    // This println! shows the code that was generated by this macro when compiling
    println!("--------------------\n{}\n------------\n\n", output);

    output
}
