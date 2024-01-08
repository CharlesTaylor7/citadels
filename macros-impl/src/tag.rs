use ::proc_macro2::TokenStream;
use ::quote::{format_ident, quote};
use ::syn::{Data, DeriveInput, Fields};

pub fn derive(ast: DeriveInput) -> TokenStream {
    let (vis, enum_name, variants) = match ast {
       DeriveInput { vis, ident, data: Data::Enum(data), .. }  => {
           (vis, ident, data.variants)
       }
       _ => {
           panic!("Tag can only be derived for enums")
       }
    };

    let tag_enum_name = format_ident!("{}Tag", enum_name);

    let mut tags = Vec::with_capacity(variants.len());
  //  let mut arg_structs: Vec<TokenStream> = Vec::with_capacity(variants.len());

    for mut variant in variants.into_iter() {
        variant.fields = Fields::Unit;
        tags.push(variant);
        /*
        if let Fields::Named(fields) = variant.fields {
            println!("{:#?}", variant.ident);
            let name = format_ident!("{}Args", variant.ident);
            let fields = fields.named;
            //.into_iter().collect();
            arg_structs.push(quote! {
                #vis struct #name {
                    #fields
                }
            })
 }
        */
    }

    quote! {
        #[derive(Clone, Copy, Debug, PartialEq, Eq)]
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
