use proc_macro2::{Ident, Span, TokenStream};
use quote::quote;
use syn::parse::{Parse, ParseStream};

pub fn alphabet(prefix: &'static str) -> impl Iterator<Item = String> + Clone {
    ('A' ..= 'Z').cycle().zip(0 ..).map(move |(c, i)| {
        let suffix = i / 26;
        let suffix = if suffix > 0 {
            (suffix - 1).to_string()
        } else {
            "".to_string()
        };
        format!("{}{}{}", prefix, c, suffix)
    })
}

pub fn idents(depth: usize, prefix: Option<&'static str>) -> impl Iterator<Item = Ident> + Clone {
    let prefix = prefix.unwrap_or("");
    alphabet(prefix)
        .take(depth)
        .map(|x| Ident::new(x.as_str(), Span::call_site()))
}

pub fn tuple_type(idents: impl Iterator<Item = Ident>) -> TokenStream {
    quote! { (#(#idents),*,) }
}

pub fn parsers_where(
    inputs: impl Iterator<Item = Ident>,
    outputs: impl Iterator<Item = Ident>,
) -> impl Iterator<Item = TokenStream> {
    inputs.zip(outputs).map(|(i, o)| {
        quote! {
            #i: Fn(&mut Vis) -> Result<#o, SyntaxErrors>
        }
    })
}

pub mod impls {
    use syn::parse::{Parse, ParseStream};

    pub struct MacroInput {
        pub depth: usize,
    }

    impl Parse for MacroInput {
        fn parse(input: ParseStream) -> syn::parse::Result<Self> {
            let lit = input.parse::<syn::LitInt>()?;
            let depth = lit.base10_parse()?;
            if depth == 0 {
                return Err(syn::Error::new(lit.span(), "depth must be non-zero"));
            }
            Ok(MacroInput { depth })
        }
    }
}
