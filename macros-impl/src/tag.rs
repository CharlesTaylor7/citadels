use ::proc_macro2::TokenStream;
use ::quote::{format_ident, quote};
use syn::{Meta, Attribute, MetaList};
use ::syn::{Data, DeriveInput, Fields};

pub fn derive(ast: DeriveInput) -> TokenStream {
    let (vis, enum_name, variants, attrs) = match ast {
       DeriveInput { vis, ident, data: Data::Enum(data), attrs, .. }  => {
           (vis, ident, data.variants, attrs)
       }
       _ => {
           panic!("Tag can only be derived for enums")
       }
    };

    let trait_list = if 
        let Some(
            Attribute { 
                meta: Meta::List(
                    MetaList {  
                        tokens, 
                        ..
                    }
                ), 
            .. }
        ) = attrs.first() {
        quote! {
            #[derive(#tokens, Clone, Copy, PartialEq, Eq, std::fmt::Debug)]
        }
    }
    else {
        quote! {
            #[derive(Clone, Copy, PartialEq, Eq, std::fmt::Debug)]
        }
    };

    let tag_enum_name = format_ident!("{}Tag", enum_name);

    let mut tags = Vec::with_capacity(variants.len());

    for mut variant in variants.into_iter() {
        variant.fields = Fields::Unit;
        tags.push(variant);
    }

        //[derive(Clone, Copy, PartialEq, Eq, std::fmt::Debug)]
    quote! {
        #trait_list
        #vis enum #tag_enum_name {
            #( #tags, )*
        }
        impl std::fmt::Display for #tag_enum_name {
             fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                write!(f, "{:#?}", self)
            }
        }

        impl Tag for #enum_name {
            type Tag = #tag_enum_name;
            fn tag(&self) -> Self::Tag {
                match self {
                    #(
                        #enum_name::#tags { .. } => #tag_enum_name::#tags,
                    )*
                }
            }
        }
    }
}
