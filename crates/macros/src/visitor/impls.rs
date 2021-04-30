use proc_macro2::{Ident, Span, TokenStream};
use quote::quote;
use syn::parse::{Parse, ParseStream};

pub fn alphabet() -> impl Iterator<Item = String> {
    ('A' ..= 'Z').cycle().zip(0 ..).map(|(c, i)| {
        let suffix = i / 26;
        let suffix = if suffix > 0 {
            (suffix - 1).to_string()
        } else {
            "".to_string()
        };
        format!("{}{}", c, suffix)
    })
}

pub fn tuple_types_impl(depth: usize) -> impl Iterator<Item = Ident> {
    alphabet()
        .take(depth)
        .take(depth)
        .map(|x| Ident::new(x.as_str(), Span::call_site()))
}

fn tuple_types_for_inner(depth: usize) -> impl Iterator<Item = Ident> {
    alphabet()
        .take(depth)
        .take(depth)
        .map(|x| Ident::new(x.as_str(), Span::call_site()))
}

pub fn tuple_types_for(depth: usize) -> TokenStream {
    let tuple_types_for_inner = tuple_types_for_inner(depth);
    match depth {
        0 => {
            quote! { () }
        },
        _ => {
            quote! { (#(#tuple_types_for_inner),*,) }
        },
    }
}

pub fn tuple_types_where(depth: usize) -> impl Iterator<Item = TokenStream> {
    alphabet().take(depth).take(depth).map(|x| {
        let ident = Ident::new(x.as_str(), Span::call_site());
        quote! {
            #ident: Fn(&mut Vis) -> Result<(), SyntaxErrors>
        }
    })
}

pub struct MacroInput {
    pub depth: usize,
}

impl Parse for MacroInput {
    fn parse(input: ParseStream) -> syn::parse::Result<Self> {
        let depth = input.parse::<syn::LitInt>()?.base10_parse()?;
        Ok(MacroInput { depth })
    }
}
