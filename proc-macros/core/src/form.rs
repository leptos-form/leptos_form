#![allow(unused)]

use ::convert_case::*;
use ::darling::{
    ast::NestedMeta,
    util::{Flag, SpannedValue},
    FromDeriveInput, FromField, FromMeta,
};
use ::derive_more::*;
use ::itertools::Itertools;
use ::proc_macro2::{Span, TokenStream};
use ::quote::{format_ident, quote, ToTokens, TokenStreamExt};
use ::std::borrow::Cow;
use ::std::ops::Deref;
use ::syn::parse::{Error, Parse, ParseStream};
use ::syn::parse2;
use ::syn::punctuated::Punctuated;
use ::syn::spanned::Spanned;

#[derive(Clone, Debug, FromDeriveInput)]
#[darling(
    attributes(form),
    forward_attrs(allow, doc, cfg),
    supports(struct_named, struct_tuple),
    and_then = "Self::one_component_kind"
)]
struct FormOpts {
    action: Option<Action>,
    class: Option<syn::LitStr>,
    component: Option<SpannedValue<Flag>>,
    error: Option<SpannedValue<ErrorHandler>>,
    field_class: Option<syn::LitStr>,
    groups: Option<Groups>,
    internal: Option<bool>,
    id: Option<syn::LitStr>,
    island: Option<SpannedValue<Flag>>,
    label: FormLabel,
    submit: Option<syn::Expr>,
    // forwarded data
    vis: syn::Visibility,
    ident: syn::Ident,
    data: darling::ast::Data<(), SpannedValue<FormField>>,
}

impl FormOpts {
    fn one_component_kind(self) -> Result<Self, darling::Error> {
        if self.component.unwrap_or_default().is_present() && self.island.unwrap_or_default().is_present() {
            Err(darling::Error::custom("Cannot set component and island")
                .with_span(&self.island.unwrap_or_default().span()))
        } else {
            Ok(self)
        }
    }
}

#[derive(Clone, Debug, Default, FromMeta, IsVariant)]
enum FormLabel {
    #[darling(rename = "none")]
    None,
    #[darling(rename = "default")]
    #[default]
    Default,
    #[darling(rename = "adjacent")]
    Adjacent {
        container: Container,
        id: Option<syn::LitStr>,
        class: Option<syn::LitStr>,
        rename_all: Option<LabelCase>,
    },
    #[darling(rename = "wrap")]
    Wrap {
        id: Option<syn::LitStr>,
        class: Option<syn::LitStr>,
        rename_all: Option<LabelCase>,
    },
}

#[derive(Clone, Debug)]
struct Groups(Vec<Container>);

#[derive(Clone, Debug, Default, FromMeta)]
enum ErrorHandler {
    Component(syn::Ident),
    Container(Container),
    #[default]
    Default,
    None,
    Raw,
}

#[derive(Clone, Debug, FromMeta)]
struct Container {
    tag: syn::Ident,
    id: Option<syn::LitStr>,
    class: Option<syn::LitStr>,
}

#[derive(Clone, Debug)]
enum Action {
    Path {
        server_fn_path: syn::Path,
        arg: syn::Ident,
        url: Option<syn::LitStr>,
    },
    Url(syn::LitStr),
}

#[derive(Clone, Debug, FromField)]
#[darling(attributes(form))]
struct FormField {
    vis: syn::Visibility,
    ident: Option<syn::Ident>,
    ty: syn::Type,
    class: Option<syn::LitStr>,
    config: Option<syn::Expr>,
    el: Option<syn::Type>,
    error: Option<SpannedValue<ErrorHandler>>,
    group: Option<SpannedValue<usize>>,
    id: Option<syn::LitStr>,
    label: Option<FieldLabel>,
}

#[derive(Clone, Debug, Default, FromMeta, IsVariant)]
enum FieldLabel {
    #[darling(rename = "none")]
    None,
    #[darling(rename = "default")]
    #[default]
    Default,
    #[darling(rename = "adjacent")]
    Adjacent {
        container: Option<Container>,
        id: Option<syn::LitStr>,
        class: Option<syn::LitStr>,
        value: Option<syn::LitStr>,
    },
    #[darling(rename = "wrap")]
    Wrap {
        id: Option<syn::LitStr>,
        class: Option<syn::LitStr>,
        value: Option<syn::LitStr>,
    },
}

impl FromMeta for Groups {
    fn from_list(items: &[NestedMeta]) -> Result<Self, darling::Error> {
        Ok(Self(
            items
                .iter()
                .map(|item| match item {
                    NestedMeta::Meta(meta) => Container::from_meta(meta),
                    NestedMeta::Lit(lit) => {
                        Err(darling::Error::custom("expected argument of the form `container(..)`")
                            .with_span(&lit.span()))
                    }
                })
                .collect::<Result<Vec<_>, _>>()?,
        ))
    }
}

impl FromMeta for Action {
    fn from_value(value: &syn::Lit) -> Result<Self, darling::Error> {
        Ok(match value {
            syn::Lit::Str(lit_str) => Self::Url(lit_str.clone()),
            _ => return Err(darling::Error::unexpected_lit_type(value)),
        })
    }
    fn from_expr(expr: &syn::Expr) -> Result<Self, darling::Error> {
        use darling::Error;

        let parse_fn_call = |expr_call: &syn::ExprCall| -> Result<(syn::Path, syn::Ident), Error> {
            let server_fn_path = match &*expr_call.func {
                syn::Expr::Path(expr_path) => expr_path.path.clone(),
                _ => return Err(Error::unexpected_expr_type(&expr_call.func)),
            };
            let first_arg = expr_call.args.first().ok_or_else(|| {
                Error::custom("expected an argument to the function call").with_span(&expr_call.args.span())
            })?;
            let arg = match first_arg {
                syn::Expr::Path(expr_path) => expr_path
                    .path
                    .get_ident()
                    .ok_or_else(|| Error::custom("only idents are supported here").with_span(&expr_path.span()))?
                    .clone(),
                _ => return Err(Error::unexpected_expr_type(first_arg)),
            };
            Ok((server_fn_path, arg))
        };

        Ok(match expr {
            syn::Expr::Call(expr_call) => {
                let (server_fn_path, arg) = parse_fn_call(expr_call)?;
                Self::Path {
                    server_fn_path,
                    arg,
                    url: None,
                }
            }
            syn::Expr::Tuple(expr_tuple) => {
                if expr_tuple.elems.len() != 2 {
                    return Err(Error::custom(r#"ActionForm cannot be used without full specification like so: `action = (server_fn_path(data), "/api/url")`"#).with_span(&expr_tuple.elems.span()));
                }
                let first = expr_tuple.elems.first().unwrap();
                let second = expr_tuple.elems.last().unwrap();

                let (server_fn_path, arg) = if let syn::Expr::Call(expr_call) = first {
                    parse_fn_call(expr_call)
                } else {
                    Err(Error::unexpected_expr_type(first))
                }?;

                let url = if let syn::Expr::Lit(syn::ExprLit {
                    lit: syn::Lit::Str(lit_str),
                    ..
                }) = second
                {
                    Ok(lit_str.clone())
                } else {
                    Err(Error::unexpected_expr_type(second))
                }?;

                Self::Path {
                    server_fn_path,
                    arg,
                    url: Some(url),
                }
            }
            _ => return Err(Error::unexpected_expr_type(expr)),
        })
    }
}

#[derive(Clone, Copy, Debug, FromMeta)]
enum LabelCase {
    #[darling(rename = "camelCase")]
    Camel,
    #[darling(rename = "kebab-case")]
    Kebab,
    #[darling(rename = "lower case")]
    Lower,
    #[darling(rename = "PascalCase")]
    Pascal,
    #[darling(rename = "snake_case")]
    Snake,
    #[darling(rename = "Title Case")]
    Title,
    #[darling(rename = "Train-Case")]
    Train,
    #[darling(rename = "UPPER CASE")]
    Upper,
    #[darling(rename = "UPPER-KEBAB-CASE")]
    UpperKebab,
    #[darling(rename = "UPPER_SNAKE_CASE")]
    UpperSnake,
}

pub fn derive_form(tokens: TokenStream) -> Result<TokenStream, Error> {
    let form_opts = FormOpts::from_derive_input(&parse2(tokens)?)?;
    let FormOpts {
        action,
        class: form_class,
        component,
        error: form_error_handler,
        field_class,
        groups,
        id: form_id,
        internal,
        island,
        label: form_label,
        submit,
        vis,
        ident,
        data,
    } = form_opts;

    let config_var_ident = format_ident!("config");
    let props_ident = format_ident!("props");
    let signal_ident = format_ident!("__{ident}Signal");
    let config_ident = format_ident!("__{ident}Config");
    let err_ident = format_ident!("__{ident}Errors");
    let error_ident = format_ident!("error");

    let is_internal = internal.unwrap_or_default();

    let component_ident = if component.unwrap_or_default().is_present() {
        Some(format_ident!("component"))
    } else if island.unwrap_or_default().is_present() {
        Some(format_ident!("island"))
    } else {
        None
    };

    let leptos_krate: syn::Path = parse2(match is_internal {
        true => quote!(::leptos),
        false => quote!(::leptos_form::internal::leptos),
    })?;
    let leptos_router_krate: syn::Path = parse2(match is_internal {
        true => quote!(::leptos_router),
        false => quote!(::leptos_form::internal::leptos_router),
    })?;
    let leptos_form_krate: syn::Path = parse2(match is_internal {
        true => quote!(crate),
        false => quote!(::leptos_form),
    })?;

    let fields = data.take_struct().unwrap();

    let (field_groups, field_axs, field_tys, field_el_tys, configs, signal_field_idents): (
        Vec<_>,
        Vec<_>,
        Vec<_>,
        Vec<_>,
        Vec<_>,
        Vec<_>,
    ) = fields
        .iter()
        .enumerate()
        .map(|(i, field)| {
            let field = field.deref();
            let field_ax = field
                .ident
                .as_ref()
                .map(|x| quote!(#x))
                .unwrap_or_else(|| i.to_string().parse().unwrap());
            let field_ty = &field.ty;
            let field_el_ty = field_el_ty(&leptos_form_krate, field);

            let config = field.config.clone().unwrap_or_else(|| {
                parse2(quote!(
                    <#field_ty as #leptos_form_krate::FormField<#field_el_ty>>::Config::default()
                ))
                .unwrap()
            });

            let signal_field_ident = if let Some(ident) = field.ident.clone() {
                ident
            } else {
                format_ident!("_{i}")
            };

            Ok((field.group, field_ax, field_ty, field_el_ty, config, signal_field_ident))
        })
        .collect::<Result<Vec<_>, Error>>()?
        .into_iter()
        .multiunzip();

    let tuple_err_fields = fields.iter().map(|_| quote!(pub Option<#leptos_form_krate::FormError>));

    let (build_props, build_props_idents, field_id_idents, field_view_idents, error_view_idents): (Vec<_>, Vec<_>, Vec<_>, Vec<_>, Vec<_>) = fields
        .iter()
        .enumerate()
        .map(|(i, spanned)| {
            let field = spanned.deref();
            let field_ty = &field_tys[i];
            let field_el_ty = &field_el_tys[i];

            let id = field.id.clone().map(|x| x.value());
            let class = field.class.clone().or_else(|| field_class.clone()).into_iter();

            let config = field.config.clone() .unwrap_or_else(|| parse2(quote!(
                <#field_ty as #leptos_form_krate::FormField<#field_el_ty>>::Config::default()
            )).unwrap());

            let field_ax = &field_axs[i];
            let field_name = field_ax.to_string();
            let field_id = id.as_ref().unwrap_or(&field_name);

            let field_id = if id.is_none() { field_id.to_case(Case::Kebab) } else { field_id.to_string() };

            // TODO: implement serde derived name case conversion

            let build_props_ident = format_ident!("_{field_ax}_props");
            let field_id_ident = format_ident!("_{field_ax}_id");
            let field_name_ident = format_ident!("_{field_ax}_name");
            let field_view_ident = format_ident!("_{field_ax}_view");
            let error_view_ident = format_ident!("_{field_ax}_error");

            let rendered_error = render_error(&leptos_krate, form_error_handler.as_ref(), field.error.as_ref(), &error_ident)?;

            Ok((
                quote!(
                    let #field_id_ident = #leptos_form_krate::format_form_id(
                        #props_ident.id.as_ref(),
                        #field_id
                    );
                    let #field_name_ident = #leptos_form_krate::format_form_name(
                        Some(&#props_ident.name),
                        #field_name
                    );
                    let #build_props_ident = #leptos_form_krate::RenderProps::builder()
                        .id(#field_id_ident.clone())
                        .name(#field_name_ident.clone())
                        #(.class(Oco::Borrowed(#class)))*
                        .signal(#props_ident.signal.#field_ax.clone())
                        .config(#config)
                        .build();

                    let #error_view_ident = move || <#field_ty as #leptos_form_krate::FormField<#field_el_ty>>::with_error(&#build_props_ident.signal, |error| match error {
                        Some(form_error) => {
                            let #error_ident = format!("{form_error}");
                            #leptos_krate::IntoView::into_view(#rendered_error)
                        },
                        None => #leptos_krate::View::default(),
                    });
                    let #field_view_ident = move || <#field_ty as #leptos_form_krate::FormComponent<#field_el_ty>>::render(#build_props_ident.clone());
                ),
                build_props_ident,
                field_id_ident,
                field_view_ident,
                error_view_ident,
            ))
        })
        .collect::<Result<Vec<_>, Error>>()?
        .into_iter()
        .multiunzip();

    let wrapped_field_views = fields
        .iter()
        .enumerate()
        .map(|(i, spanned)| {
            wrap_field(
                i,
                &form_label,
                spanned,
                &field_id_idents[i],
                &field_view_idents[i],
                &error_view_idents[i],
            )
        })
        .collect::<Result<Vec<_>, Error>>()?;

    let rendered_fields = match groups {
        Some(Groups(containers)) => {
            let mut groups = vec![vec![]; containers.len()];
            let mut ungrouped = vec![];
            for (field_group, wrapped_field_view) in field_groups.into_iter().zip(wrapped_field_views.into_iter()) {
                match field_group {
                    Some(field_group) => match *field_group < containers.len() {
                        true => groups[*field_group].push(wrapped_field_view),
                        false => return Err(Error::new(field_group.span(), "group index out of range")),
                    },
                    None => ungrouped.push(wrapped_field_view),
                };
            }
            let rendered_groups = containers
                .into_iter()
                .zip(groups)
                .map(|(Container { tag, id, class }, group)| {
                    let id = id.into_iter();
                    let class = class.into_iter();
                    quote!(
                        <#tag #(id=#id)* #(class=#class)*>
                            #(#group)*
                        </#tag>
                    )
                });

            quote!(
                #(#rendered_groups)*
                #(#ungrouped)*
            )
        }
        None => quote!(#(#wrapped_field_views)*),
    };

    let component_tokens = component_ident
        .map(|component_ident| {
            let id = form_id.iter().collect_vec();
            let class = form_class.iter().collect_vec();
            let submit = submit.iter().collect_vec();
            let props_id = form_id.as_ref().map(|id| quote!(#id)).unwrap_or_else(|| quote!(None));

            let action_ident = format_ident!("action");
            let props_signal_ident = format_ident!("signal");

            let parse_error_handler_ident = format_ident!("parse_error_handler");

            let (tag_import, action_def, open_tag, close_tag, props_name) = match action {
                Some(Action::Path { server_fn_path, arg, url }) => (
                    quote!(use #leptos_router_krate::Form;),
                    Some(quote!(let #action_ident = #leptos_krate::create_action(|data: &#ident| #server_fn_path(data.clone()));)),
                    {
                        let url = url.as_ref().map(|url| Ok(quote!(#url))).unwrap_or_else(|| {
                            let server_fn_ident = &server_fn_path.segments.last().ok_or_else(|| Error::new_spanned(&server_fn_path, "no function name found"))?.ident;
                            let url = format!("/api/{server_fn_ident}");
                            Ok::<_, Error>(quote!(#url))
                        })?;

                        quote!(<Form action=#url #(id=#id)* #(class=#class)* on:submit=move |ev| {
                            use wasm_bindgen::UnwrapThrowExt;
                            ev.prevent_default();
                            let data = match #props_signal_ident.with(|props| <#ident as FormField<#leptos_krate::View>>::try_from_signal(props.signal, &config)) {
                                Ok(parsed) => parsed,
                                Err(err) => return #parse_error_handler_ident(err),
                            };
                            action.dispatch(data);
                        }>)
                    },
                    quote!(</Form>),
                    { let arg = format!("{arg}"); quote!(#arg) },
                ),
                Some(Action::Url(url)) => (
                    quote!(use #leptos_router_krate::Form;),
                    None,
                    quote!(<Form action=#url #(id=#id)* #(class=#class)*>),
                    quote!(</Form>),
                    quote!(""),
                ),
                None => {
                    return Err(Error::new(
                        Span::call_site(),
                        "component forms must specify an action attribute",
                    ))
                }
            };
            let action_def = action_def.into_iter();

            let mod_ident = format_ident!("{}", format!("leptos_form_component_{ident}").to_case(Case::Snake));

            let props_builder = quote!(
                #leptos_form_krate::RenderProps::builder()
                    .id(#props_id)
                    .name(#props_name)
                    .signal(initial.clone().into_signal(&config))
                    .config(config.clone())
                    .build()
            );

            let pound = "#".parse::<TokenStream>().unwrap();
            let tokens = quote!(
                #vis use #mod_ident::*;

                #[allow(unused_imports)]
                mod #mod_ident {
                    use super::*;
                    use #leptos_form_krate::{FormField, components::FormSubmissionHandler};
                    use #leptos_krate::{IntoAttribute, IntoView};
                    #tag_import

                    #pound[#leptos_krate::#component_ident]
                    #vis fn #ident(initial: #ident) -> impl IntoView {
                        #(#action_def)*

                        let config = #config_ident { #(#field_axs: #configs,)* };

                        let #props_signal_ident = #leptos_krate::create_rw_signal(#props_builder);

                        let #parse_error_handler_ident = |err: #leptos_form_krate::FormError| logging::error!("{err}");

                        #leptos_krate::create_effect(move |_| {
                            if let Some(Ok(_)) = #action_ident.value().get() {
                                let config = #config_ident { #(#field_axs: #configs,)* };
                                let new_props = #props_builder;
                                #props_signal_ident.update(move |x| *x = new_props);
                            }
                        });

                        #leptos_krate::view! {
                            #open_tag
                                {move || <#ident as #leptos_form_krate::FormComponent<#leptos_krate::View>>::render(#props_signal_ident.get())}
                                #({#leptos_krate::#submit})*
                                <FormSubmissionHandler action={#action_ident} />
                            #close_tag
                        }
                    }
                }
            );
            Ok(tokens)
        })
        .transpose()?;

    let tokens = quote!(
        #[derive(Clone, Copy, Debug)]
        pub struct #signal_ident {
            #(pub #signal_field_idents: <#field_tys as #leptos_form_krate::FormField<#field_el_tys>>::Signal,)*
        }

        #[derive(Clone, Debug, Default)]
        pub struct #config_ident {
            #(pub #signal_field_idents: <#field_tys as #leptos_form_krate::FormField<#field_el_tys>>::Config,)*
        }

        #[derive(Clone, Debug, Default)]
        pub struct #err_ident {
            #(pub #signal_field_idents: Option<#leptos_form_krate::FormError>,)*
        }

        impl ::core::convert::AsRef<#signal_ident> for #signal_ident {
            fn as_ref(&self) -> &Self {
                self
            }
        }

        impl ::core::convert::AsMut<#signal_ident> for #signal_ident {
            fn as_mut(&mut self) -> &mut Self {
                self
            }
        }

        impl #leptos_form_krate::DefaultHtmlElement for #ident {
            type El = #leptos_krate::View;
        }

        impl #leptos_form_krate::FormField<#leptos_krate::View> for #ident {
            type Config = #config_ident;
            type Signal = #signal_ident;

            fn default_signal() -> Self::Signal {
                #signal_ident {
                    #(#field_axs: <#field_tys as #leptos_form_krate::FormField<#field_el_tys>>::default_signal() ,)*
                }
            }
            fn is_default_value(signal: &Self::Signal) -> bool {
                #(
                    <#field_tys as #leptos_form_krate::FormField<#field_el_tys>>::is_default_value(&signal.#field_axs)
                )&&*
            }
            fn into_signal(self, #config_var_ident: &Self::Config) -> Self::Signal {
                #signal_ident {
                    #(#field_axs: <#field_tys as #leptos_form_krate::FormField<#field_el_tys>>::into_signal(self.#field_axs, &#config_var_ident.#field_axs) ,)*
                }
            }
            fn try_from_signal(signal: Self::Signal, #config_var_ident: &Self::Config) -> Result<Self, #leptos_form_krate::FormError> {
                Ok(#ident {
                    #(#field_axs: <#field_tys as #leptos_form_krate::FormField<#field_el_tys>>::try_from_signal(signal.#field_axs, &#config_var_ident.#field_axs)? ,)*
                })
            }
            fn with_error<O>(_: &Self::Signal, f: impl FnOnce(Option<&#leptos_form_krate::FormError>) -> O) -> O {
                f(None)
            }
        }

        impl #leptos_form_krate::FormComponent<#leptos_krate::View> for #ident {
            #[allow(unused_imports)]
            fn render(#props_ident: #leptos_form_krate::RenderProps<Self::Signal, Self::Config>) -> impl #leptos_krate::IntoView {
                use #leptos_krate::{IntoAttribute, IntoView};

                #(#build_props)*

                #leptos_krate::view! {
                    #rendered_fields
                }
            }
        }

        #component_tokens
    );

    Ok(tokens)
}

fn field_el_ty(leptos_form_krate: &syn::Path, field: &FormField) -> syn::Type {
    let ty = &field.ty;
    field
        .el
        .clone()
        .unwrap_or_else(|| parse2(quote!(<#ty as #leptos_form_krate::DefaultHtmlElement>::El)).unwrap())
}

fn signal_field_ty(
    leptos_krate: &syn::Path,
    leptos_form_krate: &syn::Path,
    field: &FormField,
    el_ty: &syn::Type,
) -> Result<syn::Type, Error> {
    let ty = &field.ty;
    parse2(quote!(<#ty as #leptos_form_krate::FormField<#el_ty>>::Signal))
}

fn render_error(
    leptos_krate: &syn::Path,
    form_error_handler: Option<&SpannedValue<ErrorHandler>>,
    field_error_handler: Option<&SpannedValue<ErrorHandler>>,
    error_ident: &syn::Ident,
) -> Result<TokenStream, Error> {
    use ErrorHandler as EH;

    let form_eh = form_error_handler
        .map(|x| Cow::Borrowed(x.deref()))
        .unwrap_or(Cow::Owned(EH::Default));
    let field_eh = field_error_handler
        .map(|x| Cow::Borrowed(x.deref()))
        .unwrap_or(Cow::Owned(EH::Default));

    Ok(match (&*form_eh, &*field_eh) {
        (EH::None, EH::Default) => quote!(#leptos_krate::View::default()),
        (EH::Default, EH::Default) => quote!(#leptos_krate::view! { <span style="color: red">{#error_ident}</span> }),
        (EH::Component(component), EH::Default) => quote!(#leptos_krate::view! { <#component error=#error_ident /> }),
        (EH::Container(Container { tag, id, class }), EH::Default) => {
            let id = id.iter();
            let class = class.iter();
            quote!(#leptos_krate::view! { <#tag #(id=#id)* #(class=#class)*>{#error_ident}</#tag> })
        }
        (EH::Raw, EH::Default) => quote!({#error_ident}),
        (_, EH::None) => quote!(#leptos_krate::View::default()),
        (_, EH::Default) => quote!(#leptos_krate::view! { <span style="color: red">{#error_ident}</span> }),
        (_, EH::Component(component)) => quote!(#leptos_krate::view! { <#component error=#error_ident /> }),
        (_, EH::Container(Container { tag, id, class })) => {
            let id = id.iter();
            let class = class.iter();
            quote!(#leptos_krate::view! { <#tag #(id=#id)* #(class=#class)*>{#error_ident}</#tag> })
        }
        (_, EH::Raw) => quote!({#error_ident}),
    })
}

fn wrap_field(
    i: usize,
    form_label: &FormLabel,
    spanned: &SpannedValue<FormField>,
    field_id_ident: &syn::Ident,
    field_view_ident: &syn::Ident,
    error_view_ident: &syn::Ident,
) -> Result<TokenStream, Error> {
    let field_view = quote!({#field_view_ident});
    let error_view = quote!({#error_view_ident});
    let field = spanned.deref();
    let (container, id, class, rename_all, value) =
        match (field.label.as_ref().unwrap_or(&FieldLabel::Default), form_label) {
            (FieldLabel::None, _) => return Ok(field_view),
            (FieldLabel::Default, FormLabel::None) => return Ok(field_view),
            (FieldLabel::Default, FormLabel::Default) => (None, None, None, None, None),
            (
                FieldLabel::Default,
                FormLabel::Adjacent {
                    container,
                    id,
                    class,
                    rename_all,
                },
            ) => (Some(container), id.as_ref(), class.as_ref(), rename_all.as_ref(), None),
            (FieldLabel::Default, FormLabel::Wrap { id, class, rename_all }) => {
                (None, id.as_ref(), class.as_ref(), rename_all.as_ref(), None)
            }
            (
                FieldLabel::Adjacent {
                    container,
                    id,
                    class,
                    value,
                },
                FormLabel::Adjacent {
                    container: container_form,
                    id: id_form,
                    class: class_form,
                    rename_all,
                },
            ) => (
                Some(container.as_ref().unwrap_or(container_form)),
                id.as_ref().or(id_form.as_ref()),
                class.as_ref().or(class_form.as_ref()),
                if value.is_none() { rename_all.as_ref() } else { None },
                value.as_ref(),
            ),
            (
                FieldLabel::Adjacent {
                    container,
                    id,
                    class,
                    value,
                },
                FormLabel::Wrap {
                    id: id_form,
                    class: class_form,
                    rename_all,
                },
            ) => (
                Some(
                    container
                        .as_ref()
                        .ok_or_else(|| Error::new(spanned.span(), "a container tag must be specified"))?,
                ),
                id.as_ref().or(id_form.as_ref()),
                class.as_ref().or(class_form.as_ref()),
                if value.is_none() { rename_all.as_ref() } else { None },
                value.as_ref(),
            ),
            (
                FieldLabel::Adjacent {
                    container,
                    id,
                    class,
                    value,
                },
                FormLabel::Default,
            )
            | (
                FieldLabel::Adjacent {
                    container,
                    id,
                    class,
                    value,
                },
                FormLabel::None,
            ) => (
                Some(
                    container
                        .as_ref()
                        .ok_or_else(|| Error::new(spanned.span(), "a container tag must be specified"))?,
                ),
                id.as_ref(),
                class.as_ref(),
                None,
                value.as_ref(),
            ),
            (
                FieldLabel::Wrap { id, class, value },
                FormLabel::Adjacent {
                    id: id_form,
                    class: class_form,
                    rename_all,
                    ..
                }
                | FormLabel::Wrap {
                    id: id_form,
                    class: class_form,
                    rename_all,
                },
            ) => (
                None,
                id.as_ref().or(id_form.as_ref()),
                class.as_ref().or(class_form.as_ref()),
                if value.is_none() { rename_all.as_ref() } else { None },
                value.as_ref(),
            ),
            (FieldLabel::Wrap { id, class, value }, FormLabel::Default)
            | (FieldLabel::Wrap { id, class, value }, FormLabel::None) => {
                (None, id.as_ref(), class.as_ref(), None, value.as_ref())
            }
        };

    let label_id = id.into_iter();
    let label_class = class.into_iter();

    let label = match value {
        Some(value) => value.value(),
        None => {
            let field_ax = field
                .ident
                .as_ref()
                .map(|x| x.to_string())
                .unwrap_or_else(|| i.to_string());
            match rename_all {
                Some(label_case) => field_ax.to_case(Case::from(*label_case)),
                None => field_ax,
            }
        }
    };

    Ok(match container {
        Some(container) => {
            let tag = &container.tag;
            let container_id = container.id.as_ref().into_iter();
            let container_class = container.class.as_ref().into_iter();
            quote!(
                <#tag #(id=#container_id)* #(class=#container_class)*>
                    <label for={#field_id_ident} #(id=#label_id)* #(class=#label_class)*>
                        #label
                    </label>
                    #field_view
                    #error_view
                </#tag>
            )
        }
        None => quote!(
            <label for={#field_id_ident} #(id=#label_id)* #(class=#label_class)*>
                <div>#label</div>
                #field_view
                #error_view
            </label>
        ),
    })
}

impl From<LabelCase> for Case {
    fn from(value: LabelCase) -> Self {
        match value {
            LabelCase::Camel => Self::Camel,
            LabelCase::Kebab => Self::Kebab,
            LabelCase::Lower => Self::Lower,
            LabelCase::Pascal => Self::Pascal,
            LabelCase::Snake => Self::Snake,
            LabelCase::Title => Self::Title,
            LabelCase::Train => Self::Train,
            LabelCase::Upper => Self::Upper,
            LabelCase::UpperKebab => Self::UpperKebab,
            LabelCase::UpperSnake => Self::UpperSnake,
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn test1() -> Result<(), Error> {
        let input = quote!(
            #[derive(Form)]
            #[form(rename_labels = "Title Case")]
            pub struct MyFormData {
                pub id: Uuid,
                pub slug: String,
                pub created_at: chrono::NaiveDateTime,
                pub count: u8,
            }
        );

        let leptos_krate = quote!(::leptos_form::internal::leptos);
        let leptos_form_krate = quote!(::leptos_form);

        let expected = quote!(
            #[derive(Clone, Debug, Default)]
            pub struct __MyFormDataSignal {
                pub id: <Uuid as #leptos_form_krate::FormField<<Uuid as #leptos_form_krate::DefaultHtmlElement>::El>>::Signal,
                pub slug: <String as #leptos_form_krate::FormField<<String as #leptos_form_krate::DefaultHtmlElement>::El>>::Signal,
                pub created_at: <chrono::NaiveDateTime as #leptos_form_krate::FormField<<chrono::NaiveDateTime as #leptos_form_krate::DefaultHtmlElement>::El>>::Signal,
                pub count: <u8 as #leptos_form_krate::FormField<<u8 as #leptos_form_krate::DefaultHtmlElement>::El>>::Signal,
            }

            #[derive(Clone, Debug, Default)]
            pub struct __MyFormDataConfig {
                pub id: <Uuid as #leptos_form_krate::FormField<<Uuid as #leptos_form_krate::DefaultHtmlElement>::El>>::Config,
                pub slug: <String as #leptos_form_krate::FormField<<String as #leptos_form_krate::DefaultHtmlElement>::El>>::Config,
                pub created_at: <chrono::NaiveDateTime as #leptos_form_krate::FormField<<chrono::NaiveDateTime as #leptos_form_krate::DefaultHtmlElement>::El>>::Config,
                pub count: <u8 as #leptos_form_krate::FormField<<u8 as #leptos_form_krate::DefaultHtmlElement>::El>>::Config,
            }

            impl ::core::convert::AsRef<__MyFormDataSignal> for __MyFormDataSignal {
                fn as_ref(&self) -> &Self {
                    self
                }
            }

            impl ::core::convert::AsMut<__MyFormDataSignal> for __MyFormDataSignal {
                fn as_mut(&mut self) -> &mut Self {
                    self
                }
            }

            impl #leptos_form_krate::DefaultHtmlElement for MyFormData {
                type El = #leptos_krate::View;
            }

            impl #leptos_form_krate::FormField<#leptos_krate::View> for MyFormData {
                type Config = __MyFormDataConfig;
                type Signal = __MyFormDataSignal;
                fn into_signal(self, config: &Self::Config) -> Self::Signal {
                    __MyFormDataSignal {
                        id: <Uuid as #leptos_form_krate::FormField<<Uuid as #leptos_form_krate::DefaultHtmlElement>::El>>::into_signal(self.id, &config.id),
                        slug: <String as #leptos_form_krate::FormField<<String as #leptos_form_krate::DefaultHtmlElement>::El>>::into_signal(self.slug, &config.slug),
                        created_at: <chrono::NaiveDateTime as #leptos_form_krate::FormField<<chrono::NaiveDateTime as #leptos_form_krate::DefaultHtmlElement>::El>>::into_signal(self.created_at, &config.created_at),
                        count: <u8 as #leptos_form_krate::FormField<<u8 as #leptos_form_krate::DefaultHtmlElement>::El>>::into_signal(self.count, &config.count),
                    }
                }
                fn try_from_signal(signal: Self::Signal, config: &Self::Config) -> Result<Self, #leptos_form_krate::FormError> {
                    Ok(MyFormData {
                        id: <Uuid as #leptos_form_krate::FormField<<Uuid as #leptos_form_krate::DefaultHtmlElement>::El>>::try_from_signal(signal.id, &config.id)?,
                        slug: <String as #leptos_form_krate::FormField<<String as #leptos_form_krate::DefaultHtmlElement>::El>>::try_from_signal(signal.slug, &config.slug)?,
                        created_at: <chrono::NaiveDateTime as #leptos_form_krate::FormField<<chrono::NaiveDateTime as #leptos_form_krate::DefaultHtmlElement>::El>>::try_from_signal(signal.created_at, &config.created_at)?,
                        count: <u8 as #leptos_form_krate::FormField<<u8 as #leptos_form_krate::DefaultHtmlElement>::El>>::try_from_signal(signal.count, &config.count)?,
                    })
                }
            }

            impl<__Signal: 'static> #leptos_form_krate::FormComponent<#leptos_krate::View> for MyFormData {
                #[allow(unused_imports)]
                fn render(props: #leptos_form_krate::RenderProps<Self::Signal, Self::Config>) -> impl #leptos_krate::IntoView {
                    use #leptos_krate::{IntoAttribute, IntoView};

                    let _id_id = #leptos_form_krate::format_form_id(props.id.as_ref(), "id");
                    let _id_name = #leptos_form_krate::format_form_name(Some(&props.name), "id");
                    let _id_props = #leptos_form_krate::RenderProps::builder()
                        .id(_id_id.clone())
                        .name(_id_name.clone())
                        .signal(props.signal.id.clone())
                        .config(<Uuid as #leptos_form_krate::FormField<<Uuid as #leptos_form_krate::DefaultHtmlElement>::El>>::Config::default())
                        .build();
                    let _id_view = <Uuid as #leptos_form_krate::FormComponent<<Uuid as #leptos_form_krate::DefaultHtmlElement>::El>>::render(_id_props);

                    let _slug_id = #leptos_form_krate::format_form_id(props.id.as_ref(), "slug");
                    let _slug_name = #leptos_form_krate::format_form_name(Some(&props.name), "slug");
                    let _slug_props = #leptos_form_krate::RenderProps::builder()
                        .id(_slug_id.clone())
                        .name(_slug_name.clone())
                        .signal(props.signal.slug.clone())
                        .config(<String as #leptos_form_krate::FormField<<String as #leptos_form_krate::DefaultHtmlElement>::El>>::Config::default())
                        .build();
                    let _slug_view = <String as #leptos_form_krate::FormComponent<<String as #leptos_form_krate::DefaultHtmlElement>::El>>::render(_slug_props);

                    let _created_at_id = #leptos_form_krate::format_form_id(props.id.as_ref(), "created-at");
                    let _created_at_name = #leptos_form_krate::format_form_name(Some(&props.name), "created_at");
                    let _created_at_props = #leptos_form_krate::RenderProps::builder()
                        .id(_created_at_id.clone())
                        .name(_created_at_name.clone())
                        .signal(props.signal.created_at)
                        .config(<chrono::NaiveDateTime as #leptos_form_krate::FormField<<chrono::NaiveDateTime as #leptos_form_krate::DefaultHtmlElement>::El>>::Config::default())
                        .build();
                    let _created_at_view = <chrono::NaiveDateTime as #leptos_form_krate::FormComponent<<chrono::NaiveDateTime as #leptos_form_krate::DefaultHtmlElement>::El>>::render(_created_at_props);

                    let _count_id = #leptos_form_krate::format_form_id(props.id.as_ref(), "count");
                    let _count_name = #leptos_form_krate::format_form_name(Some(&props.name), "count");
                    let _count_props = #leptos_form_krate::RenderProps::builder()
                        .id(_count_id.clone())
                        .name(_count_name.clone())
                        .signal(props.signal.count)
                        .config(<u8 as #leptos_form_krate::FormField<<u8 as #leptos_form_krate::DefaultHtmlElement>::El>>::Config::default())
                        .build();
                    let _count_view = <u8 as #leptos_form_krate::FormComponent<<u8 as #leptos_form_krate::DefaultHtmlElement>::El>>::render(_count_props);

                    #leptos_krate::view! {
                        <label for={_id_id}>
                            <div>"Id"</div>
                            {_id_view}
                        </label>
                        <label for={_slug_id}>
                            <div>"Slug"</div>
                            {_slug_view}
                        </label>
                        <label for={_created_at_id}>
                            <div>"Created At"</div>
                            {_created_at_view}
                        </label>
                        <label for={_count_id}>
                            <div>"Count"</div>
                            {_count_view}
                        </label>
                    }
                }
            }
        );

        let output = derive_form(input)?;

        let expected = cleanup(&expected);
        let output = cleanup(&output);

        let expected = pretty(expected)?;
        let output = pretty(output)?;

        assert_eq!(expected, output);

        Ok(())
    }

    #[test]
    fn test2() -> Result<(), Error> {
        let input = quote!(
            #[derive(Form)]
            pub struct MyFormData {
                #[form(class = "hi", id = "hello-there", label = "AYO", label_class = "test")]
                pub abc_123: Uuid,
                #[form(no_label)]
                pub zz: u8,
            }
        );

        let leptos_krate = quote!(::leptos_form::internal::leptos);
        let leptos_form_krate = quote!(::leptos_form);

        let expected = quote!(
            #[derive(Clone, Debug, Default)]
            pub struct __MyFormDataSignal {
                pub abc_123: <Uuid as #leptos_form_krate::FormField<<Uuid as #leptos_form_krate::DefaultHtmlElement>::El>>::Signal,
                pub zz: <u8 as #leptos_form_krate::FormField<<u8 as #leptos_form_krate::DefaultHtmlElement>::El>>::Signal,
            }

            #[derive(Clone, Debug, Default)]
            pub struct __MyFormDataConfig {
                pub abc_123: <Uuid as #leptos_form_krate::FormField<<Uuid as #leptos_form_krate::DefaultHtmlElement>::El>>::Config,
                pub zz: <u8 as #leptos_form_krate::FormField<<u8 as #leptos_form_krate::DefaultHtmlElement>::El>>::Config,
            }

            impl ::core::convert::AsRef<__MyFormDataSignal> for __MyFormDataSignal {
                fn as_ref(&self) -> &Self {
                    self
                }
            }

            impl ::core::convert::AsMut<__MyFormDataSignal> for __MyFormDataSignal {
                fn as_mut(&mut self) -> &mut Self {
                    self
                }
            }

            impl #leptos_form_krate::DefaultHtmlElement for MyFormData {
                type El = #leptos_krate::View;
            }

            impl #leptos_form_krate::FormField<#leptos_krate::View> for MyFormData {
                type Config = __MyFormDataConfig;
                type Signal = __MyFormDataSignal;
                fn into_signal(self, config: &Self::Config) -> Self::Signal {
                    __MyFormDataSignal {
                        abc_123: <Uuid as #leptos_form_krate::FormField<<Uuid as #leptos_form_krate::DefaultHtmlElement>::El>>::into_signal(self.abc_123, &config.abc_123),
                        zz: <u8 as #leptos_form_krate::FormField<<u8 as #leptos_form_krate::DefaultHtmlElement>::El>>::into_signal(self.zz, &config.zz),
                    }
                }
                fn try_from_signal(signal: Self::Signal, config: &Self::Config) -> Result<Self, #leptos_form_krate::FormError> {
                    Ok(MyFormData {
                        abc_123: <Uuid as #leptos_form_krate::FormField<<Uuid as #leptos_form_krate::DefaultHtmlElement>::El>>::try_from_signal(signal.abc_123, &config.abc_123)?,
                        zz: <u8 as #leptos_form_krate::FormField<<u8 as #leptos_form_krate::DefaultHtmlElement>::El>>::try_from_signal(signal.zz, &config.zz)?,
                    })
                }
            }

            impl<__Signal: 'static> #leptos_form_krate::FormComponent<#leptos_krate::View> for MyFormData {
                #[allow(unused_imports)]
                fn render(props: #leptos_form_krate::RenderProps<Self::Signal, Self::Config>) -> impl #leptos_krate::IntoView {
                    use #leptos_krate::{IntoAttribute, IntoView};

                    let _abc_123_id = #leptos_form_krate::format_form_id(props.id.as_ref(), "hello-there");
                    let _abc_123_name = #leptos_form_krate::format_form_name(Some(&props.name), "abc_123");
                    let _abc_123_props = #leptos_form_krate::RenderProps::builder()
                        .id(_abc_123_id.clone())
                        .name(_abc_123_name.clone())
                        .class(Oco::Borrowed("hi"))
                        .signal(props.signal.abc_123)
                        .config(<Uuid as #leptos_form_krate::FormField<<Uuid as #leptos_form_krate::DefaultHtmlElement>::El>>::Config::default())
                        .build();
                    let _abc_123_view = <Uuid as #leptos_form_krate::FormComponent<<Uuid as #leptos_form_krate::DefaultHtmlElement>::El>>::render(_abc_123_props);

                    let _zz_id = #leptos_form_krate::format_form_id(props.id.as_ref(), "zz");
                    let _zz_name = #leptos_form_krate::format_form_name(Some(&props.name), "zz");
                    let _zz_props = #leptos_form_krate::RenderProps::builder()
                        .id(_zz_id.clone())
                        .name(_zz_name.clone())
                        .signal(props.signal.zz)
                        .config(<u8 as #leptos_form_krate::FormField<<u8 as #leptos_form_krate::DefaultHtmlElement>::El>>::Config::default())
                        .build();
                    let _zz_view = <u8 as #leptos_form_krate::FormComponent<<u8 as #leptos_form_krate::DefaultHtmlElement>::El>>::render(_zz_props);

                    #leptos_krate::view! {
                        <label for={_abc_123_id} class="test">
                            <div>"AYO"</div>
                            {_abc_123_view}
                        </label>
                        {_zz_view}
                    }
                }
            }
        );

        let output = derive_form(input)?;

        let expected = cleanup(&expected);
        let output = cleanup(&output);

        let expected = pretty(expected)?;
        let output = pretty(output)?;

        assert_eq!(expected, output);

        Ok(())
    }

    #[test]
    fn test3() -> Result<(), Error> {
        let input = quote!(
            #[derive(Form)]
            #[form(component(action = "/api/my-form-data"))]
            pub struct MyFormData {
                pub ayo: u8,
            }
        );

        let leptos_krate = quote!(::leptos_form::internal::leptos);
        let leptos_router_krate = quote!(::leptos_form::internal::leptos_router);
        let leptos_form_krate = quote!(::leptos_form);

        let expected = quote!(
            #[derive(Clone, Debug, Default)]
            pub struct __MyFormDataSignal {
                pub ayo: <u8 as #leptos_form_krate::FormField<<u8 as #leptos_form_krate::DefaultHtmlElement>::El>>::Signal,
            }

            #[derive(Clone, Debug, Default)]
            pub struct __MyFormDataConfig {
                pub ayo: <u8 as #leptos_form_krate::FormField<<u8 as #leptos_form_krate::DefaultHtmlElement>::El>>::Config,
            }

            impl ::core::convert::AsRef<__MyFormDataSignal> for __MyFormDataSignal {
                fn as_ref(&self) -> &Self {
                    self
                }
            }

            impl ::core::convert::AsMut<__MyFormDataSignal> for __MyFormDataSignal {
                fn as_mut(&mut self) -> &mut Self {
                    self
                }
            }

            impl #leptos_form_krate::DefaultHtmlElement for MyFormData {
                type El = #leptos_krate::View;
            }

            impl #leptos_form_krate::FormField<#leptos_krate::View> for MyFormData {
                type Config = __MyFormDataConfig;
                type Signal = __MyFormDataSignal;
                fn into_signal(self, config: &Self::Config) -> Self::Signal {
                    __MyFormDataSignal {
                        ayo: <u8 as #leptos_form_krate::FormField<<u8 as #leptos_form_krate::DefaultHtmlElement>::El>>::into_signal(self.ayo, &config.ayo),
                    }
                }
                fn try_from_signal(signal: Self::Signal, config: &Self::Config) -> Result<Self, #leptos_form_krate::FormError> {
                    Ok(MyFormData {
                        ayo: <u8 as #leptos_form_krate::FormField<<u8 as #leptos_form_krate::DefaultHtmlElement>::El>>::try_from_signal(signal.ayo, &config.ayo)?,
                    })
                }
            }

            impl<__Signal: 'static> #leptos_form_krate::FormComponent<#leptos_krate::View> for MyFormData {
                #[allow(unused_imports)]
                fn render(props: #leptos_form_krate::RenderProps<Self::Signal, Self::Config>) -> impl #leptos_krate::IntoView {
                    use #leptos_krate::{IntoAttribute, IntoView};

                    let _ayo_id = #leptos_form_krate::format_form_id(props.id.as_ref(), "ayo");
                    let _ayo_name = #leptos_form_krate::format_form_name(Some(&props.name), "ayo");
                    let _ayo_props = #leptos_form_krate::RenderProps::builder()
                        .id(_ayo_id.clone())
                        .name(_ayo_name.clone())
                        .signal(props.signal.ayo)
                        .config(<u8 as #leptos_form_krate::FormField<<u8 as #leptos_form_krate::DefaultHtmlElement>::El>>::Config::default())
                        .build();
                    let _ayo_view = <u8 as #leptos_form_krate::FormComponent<<u8 as #leptos_form_krate::DefaultHtmlElement>::El>>::render(_ayo_props);

                    #leptos_krate::view! {
                        <label for={_ayo_id}>
                            <div>"ayo"</div>
                            {_ayo_view}
                        </label>
                    }
                }
            }

            pub use leptos_form_component_my_form_data::*;

            #[allow(unused_imports)]
            mod leptos_form_component_my_form_data {
                use super::*;
                use #leptos_form_krate::FormField;
                use #leptos_krate::IntoView;
                use #leptos_router_krate::Form;

                #[#leptos_krate::component]
                pub fn MyFormData(initial: MyFormData) -> impl IntoView {
                    let config = __MyFormDataConfig {
                        ayo: <u8 as #leptos_form_krate::FormField<<u8 as #leptos_form_krate::DefaultHtmlElement>::El>>::Config::default(),
                    };

                    let signal = #leptos_krate::create_rw_signal(initial.into_signal(&config));
                    let props = #leptos_form_krate::RenderProps::builder()
                        .id(None)
                        .name("")
                        .signal(signal)
                        .config(config)
                        .build();
                    let view = <MyFormData as #leptos_form_krate::FormComponent<#leptos_krate::View>>::render(props);
                    #leptos_krate::view! {
                        <Form action="/api/my-form-data">
                            {view}
                        </Form>
                    }
                }
            }
        );

        let output = derive_form(input)?;

        let expected = cleanup(&expected);
        let output = cleanup(&output);

        let expected = pretty(expected)?;
        let output = pretty(output)?;

        assert_eq!(expected, output);

        Ok(())
    }

    pub fn cleanup(tokens: &TokenStream) -> String {
        let tokens = tokens.to_string();

        tokens.replace("< <", "<<").replace("> >", ">>")
    }

    pub fn pretty(cleaned_up: String) -> Result<String, Error> {
        let syntax_tree = syn::parse_file(&cleaned_up)?;
        Ok(prettyplease::unparse(&syntax_tree))
    }
}
