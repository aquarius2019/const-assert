use quote::quote;
use syn::punctuated::Punctuated;

enum GenericItem {
    Type(syn::Ident),
    Const(syn::Ident, syn::Type),
}

impl syn::parse::Parse for GenericItem {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let ident = input.parse()?;

        if input.peek(syn::Token![:]) {
            input.parse::<syn::Token![:]>()?;
            Ok(GenericItem::Const(ident, input.parse()?))
        }
        else {
            Ok(GenericItem::Type(ident))
        }
    }
}

impl quote::ToTokens for GenericItem {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        match self {
            GenericItem::Type(ident) => ident.to_tokens(tokens),
            GenericItem::Const(ident, ty ) => tokens.extend(quote! { const #ident: #ty }),
        };
    }
}

struct Params(Punctuated<GenericItem, syn::Token![,]>);

impl syn::parse::Parse for Params {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let mut params = Punctuated::new();

        input.parse::<syn::Token![|]>()?;
        loop {
            if input.peek(syn::Token![|]) {
                break;
            }

            params.push_value(input.parse()?);
            
            if input.peek(syn::Token![|]) {
                break;
            }

            params.push_punct(input.parse()?);
        }
        input.parse::<syn::Token![|]>()?;

        Ok(Self(params))
    }
}

struct ConstAssertInput {
    params: Params,
    expr: syn::Expr,
    msg: Option<proc_macro2::TokenStream>
}

impl syn::parse::Parse for ConstAssertInput {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let params = input.parse()?;
        let expr = input.parse()?;
        let msg = if input.peek(syn::Token![,]) { input.parse::<syn::Token![,]>()?; Some(input.parse()?) } else { None };

        Ok(ConstAssertInput { params, expr, msg })
    }
}

#[proc_macro]
pub fn const_assert(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let ConstAssertInput {params, expr, msg } = syn::parse_macro_input!(input as ConstAssertInput);
    let params = params.0;
    let len = params.len();
    let mut decls = Vec::with_capacity(len);
    let mut idents = Vec::with_capacity(len);
    let mut types = Vec::with_capacity(len);

    for item in params.iter() {
        decls.push(quote! { #item });

        match item {
            GenericItem::Type(ident) => {
                idents.push(quote! { #ident });
                types.push(quote! { #ident });
            },
            GenericItem::Const(ident, _) => {
                idents.push(quote! { #ident });
            },
        }
    }
    
    let decls = { let iter = decls.iter(); quote! {#(#iter),*} };
    let idents = { let iter = idents.iter(); quote! {#(#iter),*} };
    let types = { let iter = types.iter(); quote! {#(#iter),*} };
    
    quote! {
        {
            struct ConstAssert<#decls>(core::marker::PhantomData<(#types)>);

            impl <#decls> ConstAssert<#idents> {
                #[allow(unused)]
                const ASSERT: () = assert!(#expr, #msg);
            }

            ConstAssert::<#idents>::ASSERT;
        }
    }.into()
}
