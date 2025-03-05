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
            let ty = input.parse()?;
            Ok(GenericItem::Const(ident, ty))
        }
        else {
            Ok(GenericItem::Type(ident))
        }
    }
}

impl quote::ToTokens for GenericItem {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        match self {
            GenericItem::Type(ident) => tokens.extend(quote! { #ident }),
            GenericItem::Const(ident, ty ) => tokens.extend(quote! { const #ident: #ty }),
        };
    }
}

struct Params (Punctuated<GenericItem, syn::Token![,]>);

impl syn::parse::Parse for Params {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        input.parse::<syn::Token![|]>()?;

        let mut params = Punctuated::new();

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
    let ConstAssertInput {params: Params(params), expr, msg } = syn::parse_macro_input!(input as ConstAssertInput);
    
    let param_decl = {
        let params = params.iter().map(|item| { quote! { #item } });
        quote! { #(#params),*}
    };

    let param_idents = {
        let params = params.iter().map(|item| {
            match item {
                GenericItem::Type(ident) => quote! { #ident },
                GenericItem::Const(ident, _) => quote! { #ident },
            }
        });

        quote! { #(#params),*}
    };

    let type_items = {
        let items = params.iter().filter_map(|item| {
            if let GenericItem::Type(ident) = item { Some(quote!{ #ident }) } else { None }
        });

        quote! {#(#items),*}
    };

    quote! {
        {
            struct ConstAssertStruct<#param_decl>(core::marker::PhantomData<(#type_items)>);

            impl <#param_decl> ConstAssertStruct<#param_idents> {
                #[allow(unused)]
                const ASSERT: () = assert!(#expr, #msg);
            }

            ConstAssertStruct::<#param_idents>::ASSERT;
        }
        
    }.into()
}
