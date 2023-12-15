use ::convert_case::*;
use ::darling::{
    ast::{NestedMeta, Style},
    util::SpannedValue,
    FromDeriveInput, FromField, FromMeta,
};
use ::derive_more::*;
use ::itertools::Itertools;
use ::proc_macro2::{Span, TokenStream};
use ::quote::{format_ident, quote, ToTokens};
use ::std::borrow::Cow;
use ::std::ops::Deref;
use ::syn::parse::Error;
use ::syn::punctuated::Punctuated;
use ::syn::spanned::Spanned;
use ::syn::{parse2, parse_str};

#[derive(Clone, Debug, FromDeriveInput)]
#[darling(
    attributes(form),
    forward_attrs(allow, doc, cfg),
    supports(struct_named, struct_tuple),
    and_then = "Self::one_component_kind"
)]
struct FormOpts {
    component: Option<ComponentConfigSpanned>,
    error: Option<SpannedValue<ErrorHandler>>,
    field_class: Option<StringExpr>,
    field_style: Option<StringExpr>,
    groups: Option<Groups>,
    internal: Option<SpannedValue<bool>>,
    id: Option<StringExpr>,
    island: Option<ComponentConfigSpanned>,
    label: Option<FormLabel>,
    wrapper: Option<bool>,
    // forwarded fields
    vis: syn::Visibility,
    ident: syn::Ident,
    data: darling::ast::Data<(), SpannedValue<FormField>>,
}

impl FormOpts {
    fn one_component_kind(self) -> Result<Self, darling::Error> {
        if self.component.is_some() && self.island.is_some() {
            Err(darling::Error::custom("cannot set component and island").with_span(&self.island.unwrap().span()))
        } else {
            Ok(self)
        }
    }
}

#[derive(Clone, Debug)]
struct ComponentConfigSpanned {
    field: syn::Ident,
    spanned: ComponentConfig,
}

#[derive(Clone, Debug, Default, FromMeta)]
struct ComponentConfig {
    action: Option<Action>,
    cache: Option<Cache>,
    class: Option<StringExpr>,
    field_changed_class: Option<StringExpr>,
    map_submit: Option<MapSubmit>,
    name: Option<syn::Ident>,
    on_error: Option<syn::Expr>,
    on_success: Option<syn::Expr>,
    reset_on_success: Option<bool>,
    style: Option<StringExpr>,
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
        id: Option<StringExpr>,
        class: Option<StringExpr>,
        rename_all: Option<LabelCase>,
        style: Option<StringExpr>,
    },
    #[darling(rename = "wrap")]
    Wrap {
        id: Option<StringExpr>,
        class: Option<StringExpr>,
        rename_all: Option<LabelCase>,
        style: Option<StringExpr>,
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
    id: Option<StringExpr>,
    class: Option<StringExpr>,
    style: Option<StringExpr>,
}

#[derive(Clone, Debug, FromMeta)]
struct FieldLabelContainer {
    tag: Option<syn::Ident>,
    id: Option<StringExpr>,
    class: Option<StringExpr>,
    style: Option<StringExpr>,
}

#[derive(Clone, Debug, IsVariant)]
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
    class: Option<StringExpr>,
    config: Option<syn::Expr>,
    el: Option<Element>,
    error: Option<SpannedValue<ErrorHandler>>,
    group: Option<SpannedValue<usize>>,
    id: Option<StringExpr>,
    label: Option<FieldLabel>,
    style: Option<StringExpr>,
    // forwarded fields
    ident: Option<syn::Ident>,
    ty: syn::Type,
}

#[derive(Clone, Debug)]
struct Element(syn::Type);

#[derive(Clone, Debug, Default, FromMeta, IsVariant)]
enum FieldLabel {
    #[darling(rename = "adjacent")]
    Adjacent {
        container: Option<FieldLabelContainer>,
        id: Option<StringExpr>,
        class: Option<StringExpr>,
        style: Option<StringExpr>,
        value: Option<syn::LitStr>,
    },
    #[darling(rename = "default")]
    #[default]
    Default,
    #[darling(rename = "none")]
    None,
    #[darling(rename = "wrap")]
    Wrap {
        id: Option<StringExpr>,
        class: Option<StringExpr>,
        style: Option<StringExpr>,
        value: Option<syn::LitStr>,
    },
}

#[derive(Clone, Debug)]
enum MapSubmit {
    Defn(syn::ExprClosure),
    Path(syn::Path),
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

#[derive(Clone, Debug)]
enum StringExpr {
    Expr(syn::Expr),
    LitStr(String),
}

#[derive(Clone, Debug, FromMeta)]
struct Cache {
    debounce_ms: Option<syn::LitInt>,
    key: Option<syn::Expr>,
    value: CacheValue,
}

#[derive(Clone, Debug)]
struct CacheValue(syn::Expr);

pub fn derive_form(tokens: TokenStream) -> Result<TokenStream, Error> {
    let ast: syn::DeriveInput = parse2(tokens)?;

    let form_opts = FormOpts::from_derive_input(&ast)?;
    let FormOpts {
        component,
        data,
        error: form_error_handler,
        field_class,
        field_style,
        groups,
        id: form_id,
        ident,
        internal,
        island,
        label: form_label,
        vis,
        wrapper,
    } = form_opts;

    let error_ident = format_ident!("error");
    let config_var_ident = format_ident!("config");
    let props_ident = format_ident!("props");

    let signal_ident = format_ident!("__{ident}Signal");
    let config_ident = format_ident!("__{ident}Config");
    let mod_ident = format_ident!("{}", format!("leptos_form_component_{ident}").to_case(Case::Snake));

    let form_label = form_label.unwrap_or_default();

    let component_ident = if component.is_some() {
        Some(format_ident!("component"))
    } else if island.is_some() {
        Some(format_ident!("island"))
    } else {
        None
    };

    let krate = std::env::var("CARGO_PKG_NAME").unwrap();
    if let Some(internal) = internal.as_ref() {
        if krate != "leptos_form" && krate != "leptos_form_proc_macros_core" {
            return Err(Error::new(internal.span(), "unknown attribute `internal`"));
        }
    }
    let is_internal = internal.map(|x| *x).unwrap_or_default();
    let is_wrapper = wrapper.unwrap_or_default();

    let leptos_form_krate: syn::Path = parse2(match is_internal {
        true => quote!(crate),
        false => quote!(::leptos_form),
    })?;
    let leptos_krate: syn::Path = parse2(quote!(#leptos_form_krate::internal::leptos))?;
    let leptos_router_krate: syn::Path = parse2(quote!(#leptos_form_krate::internal::leptos_router))?;
    let wasm_bindgen_krate: syn::Path = parse2(quote!(#leptos_form_krate::internal::wasm_bindgen))?;
    let web_sys_krate: syn::Path = parse2(quote!(#leptos_form_krate::internal::web_sys))?;

    let fields = data.take_struct().unwrap();

    let component_ty = ident.clone();
    let signal_ty = signal_ident.clone();
    let config_ty = config_ident.clone();

    #[allow(clippy::type_complexity)]
    let (field_groups, field_axs, field_tys, field_el_tys, configs, signal_fields, config_fields): (
        Vec<_>,
        Vec<_>,
        Vec<_>,
        Vec<_>,
        Vec<_>,
        Punctuated<syn::Field, syn::token::Comma>,
        Punctuated<syn::Field, syn::token::Comma>,
    ) = fields
        .iter()
        .enumerate()
        .map(|(i, field)| {
            let field = field.deref();
            let field_ax = field
                .ident
                .as_ref()
                .map(|x| quote!(#x))
                .unwrap_or_else(|| parse_str(&i.to_string()).unwrap());
            let field_ty = &field.ty;
            let field_el_ty = field_el_ty(&leptos_form_krate, field);

            let config = field.config.clone().unwrap_or_else(|| {
                parse2(quote!(
                    <#field_ty as #leptos_form_krate::FormField<#field_el_ty>>::Config::default()
                ))
                .unwrap()
            });

            let create_field = |ident: Option<syn::Ident>, ty: syn::Type| syn::Field {
                attrs: Default::default(),
                vis: syn::Visibility::Public(Default::default()),
                mutability: syn::FieldMutability::None,
                ident,
                colon_token: Some(Default::default()),
                ty,
            };

            let field_signal_ty: syn::Type =
                parse2(quote!(<#field_ty as #leptos_form_krate::FormField<#field_el_ty>>::Signal)).unwrap();
            let field_config_ty: syn::Type =
                parse2(quote!(<#field_ty as #leptos_form_krate::FormField<#field_el_ty>>::Config)).unwrap();

            let (signal_field, config_field) = (
                create_field(field.ident.clone(), field_signal_ty),
                create_field(field.ident.clone(), field_config_ty),
            );

            (
                field.group,
                field_ax,
                field_ty,
                field_el_ty,
                config,
                signal_field,
                config_field,
            )
        })
        .multiunzip();

    let (build_props, field_id_idents, field_view_idents, error_view_idents): (Vec<_>, Vec<_>, Vec<_>, Vec<_>) = fields
        .iter()
        .enumerate()
        .map(|(i, spanned)| {
            let field = spanned.deref();
            let field_ty = &field_tys[i];
            let field_el_ty = &field_el_tys[i];

            let class = field.class.clone().or_else(|| field_class.clone()).map(StringExpr::with_oco(&leptos_krate)).into_iter();
            let style = field.style.clone().or_else(|| field_style.clone()).map(StringExpr::with_oco(&leptos_krate)).into_iter();

            let config = field.config.clone().unwrap_or_else(|| parse2(quote!(
                <#field_ty as #leptos_form_krate::FormField<#field_el_ty>>::Config::default()
            )).unwrap());

            let field_ax = &field_axs[i];
            let field_name = field_ax.to_string();

            let field_id = (StringExpr::with_oco(&leptos_krate))(field.id
                .clone()
                .unwrap_or_else(|| StringExpr::LitStr(field_name.clone()))
                .map_case(Case::Kebab));

            // TODO: implement serde derived name case conversion

            let build_props_ident = format_ident!("_{field_ax}_props");
            let field_id_ident = format_ident!("_{field_ax}_id");
            let field_name_ident = format_ident!("_{field_ax}_name");
            let field_view_ident = format_ident!("_{field_ax}_view");
            let error_view_ident = format_ident!("_{field_ax}_error");

            let rendered_error = render_error(&leptos_krate, form_error_handler.as_ref(), field.error.as_ref(), &error_ident)?;

            let field_changed_class = component
                .as_ref()
                .or(island.as_ref())
                .and_then(|x| x.spanned.field_changed_class.as_ref())
                .map(|field_changed_class| quote!(#leptos_krate::Oco::Borrowed(#field_changed_class)))
                .unwrap_or_else(|| quote!(#props_ident.field_changed_class.clone()));

            let field_id_builder = match is_wrapper {
                true => quote!(#props_ident.id),
                false => quote!(#leptos_form_krate::format_form_id(#props_ident.id.as_ref(), #field_id)),
            };
            let field_name_builder = match is_wrapper {
                true => quote!(#props_ident.name),
                false => quote!(#leptos_form_krate::format_form_name(#props_ident.name.as_ref(), #field_name)),
            };

            Ok((
                quote!(
                    let #field_id_ident = #field_id_builder;
                    let #field_name_ident = #field_name_builder;
                    let #build_props_ident = #leptos_form_krate::RenderProps::builder()
                        .id(#field_id_ident.clone())
                        .name(#field_name_ident.clone())
                        #(.class(#class))*
                        #(.style(#style))*
                        .field_changed_class(#field_changed_class)
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

                    let ty = <::std::marker::PhantomData<(#field_ty, #field_el_ty)> as Default>::default();
                    let #field_view_ident = #leptos_krate::view! { <FormField props=#build_props_ident ty=ty /> };
                ),
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
            let rendered_groups =
                containers
                    .into_iter()
                    .zip(groups)
                    .map(|(Container { tag, id, class, style }, group)| {
                        let id = id.into_iter();
                        let class = class.into_iter();
                        let style = style.into_iter();
                        quote!(
                            <#tag #(id=#id)* #(class=#class)* #(style=#style)*>
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
            let component_config = component.as_ref().or(island.as_ref()).unwrap();
            let get_component_name_invalid_span = || match component_config.spanned.name.as_ref() {
                None => Some(component_config.field.span()),
                Some(name) => match *name == ident || *name == format_ident!("_{ident}") {
                    true => Some(name.span()),
                    false => None,
                },
            };
            if fields.style == Style::Tuple {
                if let Some(span) = get_component_name_invalid_span() {
                    return Err(Error::new(
                        span,
                        format!(
                            r#"deriving Form for a tuple struct with `{}` specified additionally requires a component name, consider using `{}(name = "{ident}_")`; note that "{ident}" cannot be used as the component name because {ident} is already an item defined in the function namespace and "_{ident}" should not be used because of leptos name transformations"#,
                            component_config.field,
                            component_config.field,
                        ),
                    ));
                }
            }
            let ComponentConfig {
                action,
                cache,
                class: form_class,
                field_changed_class,
                map_submit,
                name: component_name,
                on_error,
                on_success,
                reset_on_success,
                style: form_style,
            } = &component_config.spanned;

            let component_name = component_name.as_ref().unwrap_or(&ident);
            let id = form_id.iter();
            let class = form_class.iter();
            let style = form_style.iter();
            let field_changed_class = field_changed_class.iter();

            let data_ident = format_ident!("data");
            let action_ident = format_ident!("action");
            let initial_ident = format_ident!("initial");
            let props_signal_ident = format_ident!("signal");
            let delete_from_cache_ident = cache.is_some().then_some(format_ident!("delete_from_cache"));
            let parse_error_handler_ident = format_ident!("parse_error_handler");

            let props_id = form_id.as_ref().map(|id| quote!(#leptos_krate::Oco::Borrowed(#id))).unwrap_or_else(|| quote!(None));

            let action = action.as_ref().ok_or_else(||Error::new(
                Span::call_site(),
                "component forms must specify an action attribute",
            ))?;

            let map_submit = if action.is_path() {
                match map_submit {
                    Some(MapSubmit::Defn(closure_defn)) => {
                        quote!(
                            let map_submit = #closure_defn;
                            let #data_ident = map_submit(#leptos_form_krate::FormDiff { initial: #initial_ident.clone(), current: #data_ident });
                        )
                    },
                    Some(MapSubmit::Path(path)) => quote!(
                        let #data_ident = #path(#leptos_form_krate::FormDiff { initial: #initial_ident.clone(), current: #data_ident });
                    ),
                    None => quote!(),
                }
            } else {
                quote!()
            };

            let parse_from_signal = quote!(#props_signal_ident.with(|props| <#component_ty as #leptos_form_krate::FormField<#leptos_krate::View>>::try_from_signal(props.signal, &config)));

            let _delete_from_cache_ident = delete_from_cache_ident.iter();
            let (tag_import, action_def, open_tag, close_tag, props_name) = match action {
                Action::Path { server_fn_path, arg, url } => (
                    quote!(use #leptos_router_krate::Form;),
                    Some(quote!(
                        fn server_fn_inference<T: Clone, U>(f: impl Fn(T) -> U) -> impl Fn(&T) -> U {
                            move |data: &T| f(data.clone())
                        }
                        let #action_ident = #leptos_krate::create_action(server_fn_inference(#server_fn_path));
                    )),
                    {
                        let url = url.as_ref().map(|url| Ok(quote!(#url))).unwrap_or_else(|| {
                            let server_fn_ident = &server_fn_path.segments.last().ok_or_else(|| Error::new_spanned(server_fn_path, "no function name found"))?.ident;
                            let url = format!("/api/{server_fn_ident}");
                            Ok::<_, Error>(quote!(#url))
                        })?;

                        quote!(<Form action=#url #(attr:id=#id)* #(attr:class=#class)* #(attr:style=#style)* on:submit=move |ev| {
                            ev.prevent_default();
                            let #data_ident = match #parse_from_signal {
                                Ok(parsed) => parsed,
                                Err(err) => {
                                    #parse_error_handler_ident(err);
                                    return;
                                },
                            };

                            #map_submit

                            action.dispatch(#data_ident);
                            #(#_delete_from_cache_ident())*
                        }>)
                    },
                    quote!(</Form>),
                    { let arg = format!("{arg}"); quote!(#leptos_krate::Oco::Borrowed(#arg)) },
                ),
                Action::Url(url) => (
                    quote!(use #leptos_router_krate::Form;),
                    None,
                    quote!(<Form action=#url #(attr:id=#id)* #(attr:class=#class)* #(attr:style=#style)*>),
                    quote!(</Form>),
                    quote!(""),
                ),
            };
            let action_def = action_def.into_iter();

            let props_builder = quote!(
                #leptos_form_krate::RenderProps::builder()
                    .id(#props_id)
                    .name(#props_name)
                    .signal(#initial_ident.clone().into_signal(&config, Some(#initial_ident.clone())))
                    .config(config.clone())
                    #(.field_changed_class(#leptos_krate::Oco::Borrowed(#field_changed_class)))*
                    .build()
            );

            let config_def = quote!(let config = #config_ty { #(#field_axs: #configs,)* };);

            let optional_reset_on_success_effect = if action.is_path() {
                    match reset_on_success.unwrap_or_default() {
                    true => quote!(
                        #leptos_krate::create_effect({
                            let initial = initial.clone();
                            let action_value = #action_ident.value();
                            move |prev_value| {
                                let value = action_value.get();
                                if let None|Some(Err(_)) = value.as_ref() {
                                    return None;
                                }
                                match prev_value {
                                    None|Some(None) => {
                                        #config_def
                                        let new_props = #props_builder;
                                        #props_signal_ident.update(move |props| *props = new_props);
                                        _had_reset_called.update(|x| *x = true);
                                        Some(value)
                                    },
                                    Some(Some(prev_value)) => None,
                                }
                            }
                        });
                    ),
                    false => quote!(
                        #leptos_krate::create_effect({
                            let action_value = #action_ident.value();
                            move |prev_value| {
                                let value = action_value.get();
                                if let None|Some(Err(_)) = value.as_ref() {
                                    return None;
                                }
                                match prev_value {
                                    None|Some(None) => {
                                        #props_signal_ident.with(|props| #ident::reset_initial_value(&props.signal));
                                        #props_signal_ident.with(|props| #ident::recurse(&props.signal));
                                        _had_reset_called.update(|x| *x = true);
                                        Some(value)
                                    },
                                    Some(Some(prev_value)) => None,
                                }
                            }
                        });
                    ),
                }
            } else {
                quote!()
            };

            let form_submission_handler = if action.is_path() {
                let error_view_ty = if on_error.is_some() { quote!() } else { quote!(error_view_ty={<::std::marker::PhantomData<#leptos_krate::View> as Default>::default()}) };
                let success_view_ty = if on_success.is_some() { quote!() } else { quote!(success_view_ty={<::std::marker::PhantomData<#leptos_krate::View> as Default>::default()}) };

                let on_error = on_error.iter();
                let on_success = on_success.iter();
                quote!(
                    <FormSubmissionHandler
                        action=#action_ident
                        #(on_error=Rc::new(#on_error))*
                        #(on_success=Rc::new(#on_success))*
                        #error_view_ty
                        #success_view_ty
                    />
                )
            } else {
                quote!()
            };

            let cache_effects = match cache {
                Some(Cache { debounce_ms, key, value: CacheValue(cache_value) }) => {
                    let (form_id_fmt, form_id_in_key) = match form_id.as_ref() {
                        Some(form_id) => (quote!("{}{}#{}"), quote!(#form_id,)),
                        None => (quote!("{}{}"), quote!()),
                    };
                    let key = key
                        .as_ref()
                        .map(|key| quote!(#key))
                        .unwrap_or_else(|| quote!({
                            #[cfg(target_arch = "wasm32")]
                            let key = format!(
                                #form_id_fmt,
                                web_sys::window().unwrap_throw().location().host().unwrap_throw(),
                                #leptos_router_krate::use_route().path(),
                                #form_id_in_key
                            );

                            #[cfg(not(target_arch = "wasm32"))]
                            let key = String::default();

                            key
                        }));

                    let debounce_ms = debounce_ms.as_ref().map(|x| x.base10_parse::<i32>()).transpose()?.unwrap_or(1500);

                    let cache_effects = quote!(
                        let cache = std::rc::Rc::new(#cache_value);

                        // read from cache on initial load
                        #[cfg(target_arch = "wasm32")]
                        #leptos_krate::create_local_resource(|| (), {
                            let cache = cache.clone();
                            let cache_key = #key;
                            let initial = #initial_ident.clone();
                            move |_| {
                                let cache = cache.clone();
                                let cache_key = cache_key.clone();
                                let initial = initial.clone();
                                async move {
                                    use #leptos_form_krate::cache::Cache;
                                    let item: #ident = cache.get_item(&cache_key).await.unwrap_throw()?;
                                    #props_signal_ident.update(|props| {
                                        props.signal = item.into_signal(&props.config, Some(initial));
                                    });
                                    Some(())
                                }
                            }
                        });

                        // write to cache
                        #[cfg(target_arch = "wasm32")]
                        let write_to_cache_handle = {
                            let write_signal = create_rw_signal(0u8);
                            let write_to_cache_handle = create_rw_signal(0i32);

                            let incr = move || write_signal.update(|x| *x = match *x { u8::MAX => 0, _ => *x + 1 });
                            let cb: Closure<dyn Fn()> = Closure::new(incr);

                            // update timeout to write to cache
                            #leptos_krate::create_effect(move |prev_handle| {
                                let window = #web_sys_krate::window().unwrap_throw();
                                if let Some(Some(prev_handle)) = prev_handle {
                                    window.clear_timeout_with_handle(prev_handle);
                                }
                                #props_signal_ident.with(|props| #ident::recurse(&props.signal));
                                let timeout_handle = window.set_timeout_with_callback_and_timeout_and_arguments_0(
                                    &cb.as_ref().unchecked_ref(),
                                    #debounce_ms,
                                ).unwrap_throw();
                                write_to_cache_handle.update(|x| *x = timeout_handle);
                                Some(timeout_handle)
                            });

                            let cache = cache.clone();
                            let cache_key = #key;
                            #leptos_krate::create_local_resource(move || write_signal.get(), move |_| {
                                let cache = cache.clone();
                                let cache_key = cache_key.clone();
                                async move {
                                    use #leptos_form_krate::cache::Cache;

                                    if _had_reset_called.get() {
                                        _had_reset_called.update(|x| *x = false);
                                        return;
                                    }

                                    let value = #props_signal_ident.with(|props| {
                                        let config = &props.config;
                                        #parse_from_signal
                                    }).unwrap_throw();
                                    cache.set_item(&cache_key, &value).await.unwrap_throw();
                                }
                            });

                            write_to_cache_handle
                        };

                        // remove from cache on submit
                        #[cfg(target_arch = "wasm32")]
                        let #delete_from_cache_ident = {
                            let cache_key = #key;
                            move || {
                                let window = #web_sys_krate::window().unwrap_throw();
                                let local_storage = window.local_storage().unwrap_throw().unwrap_throw();
                                local_storage.remove_item(&cache_key).unwrap_throw();
                                window.clear_timeout_with_handle(write_to_cache_handle.get());
                            }
                        };

                        #[cfg(not(target_arch = "wasm32"))]
                        let #delete_from_cache_ident = || {};
                    );
                    cache_effects
                },
                _ => quote!()
            };

            let pound = "#".parse::<TokenStream>().unwrap();
            let tokens = quote!(
                // `leptos::component` fails to compile if the return type of the component function
                // is not `impl IntoView` verbatim which is unnecessarily restrictive but not a blocker here;
                // Means we have to create a submodule which imports IntoView into scope. Note that
                // doc tests will fail if the submodule tries to import any values from super hence
                // the strange imports and double functions.
                #vis use #mod_ident::*;
                mod #mod_ident {
                    use super::*;
                    use #leptos_krate::IntoView;
                    use #wasm_bindgen_krate::{closure::Closure, JsCast, UnwrapThrowExt};

                    #[allow(unused_imports)]
                    #pound[#leptos_krate::#component_ident]
                    #vis fn #component_name(
                        mut #initial_ident: #ident,
                        #[prop(optional, into)] top: Option<#leptos_form_krate::components::LeptosFormChildren>,
                        #[prop(optional, into)] bottom: Option<#leptos_form_krate::components::LeptosFormChildren>,
                    ) -> impl IntoView {
                        use #leptos_form_krate::{FormField, components::FormSubmissionHandler};
                        use #leptos_krate::{IntoAttribute, IntoView, SignalGet, SignalUpdate, SignalWith};
                        use #wasm_bindgen_krate::UnwrapThrowExt;
                        #tag_import
                        use ::std::rc::Rc;

                        #(#action_def)*
                        #config_def

                        let #props_signal_ident = #leptos_krate::create_rw_signal(#props_builder);

                        let _had_reset_called = #leptos_krate::create_rw_signal(false);
                        #cache_effects

                        let #parse_error_handler_ident = |err: #leptos_form_krate::FormError| #leptos_krate::logging::debug_warn!("{err}");

                        #optional_reset_on_success_effect

                        let ty = <::std::marker::PhantomData<(#ident, #leptos_krate::View)> as Default>::default();

                        #leptos_krate::view! {
                            #open_tag
                                {top.map(|x| (x.0)())}
                                {move || #leptos_krate::view! { <FormField props=#props_signal_ident.get() ty=ty /> }}
                                {bottom.map(|x| (x.0)())}
                                #form_submission_handler
                            #close_tag
                        }
                    }
                }
            );
            Ok(tokens)
        })
        .transpose()?;

    let signal_struct_def = syn::ItemStruct {
        attrs: vec![syn::Attribute {
            pound_token: Default::default(),
            style: syn::AttrStyle::Outer,
            bracket_token: Default::default(),
            meta: parse2(quote!(derive(Clone, Copy, Debug)))?,
        }],
        vis: syn::Visibility::Public(Default::default()),
        struct_token: Default::default(),
        ident: signal_ident.clone(),
        fields: match fields.style {
            Style::Tuple => syn::Fields::Unnamed(syn::FieldsUnnamed {
                unnamed: signal_fields,
                paren_token: Default::default(),
            }),
            Style::Struct => syn::Fields::Named(syn::FieldsNamed {
                named: signal_fields,
                brace_token: Default::default(),
            }),
            Style::Unit => unreachable!(),
        },
        // TODO: define generics on signal struct if supported in the future
        generics: Default::default(),
        semi_token: (fields.style == Style::Struct).then_some(Default::default()),
    };

    let config_struct_def = syn::ItemStruct {
        attrs: vec![syn::Attribute {
            pound_token: Default::default(),
            style: syn::AttrStyle::Outer,
            bracket_token: Default::default(),
            meta: parse2(quote!(derive(Clone, Debug, Default)))?,
        }],
        vis: syn::Visibility::Public(Default::default()),
        struct_token: Default::default(),
        ident: config_ident.clone(),
        fields: match fields.style {
            Style::Tuple => syn::Fields::Unnamed(syn::FieldsUnnamed {
                unnamed: config_fields,
                paren_token: Default::default(),
            }),
            Style::Struct => syn::Fields::Named(syn::FieldsNamed {
                named: config_fields,
                brace_token: Default::default(),
            }),
            Style::Unit => unreachable!(),
        },
        // TODO: define generics on signal struct if supported in the future
        generics: Default::default(),
        semi_token: (fields.style == Style::Struct).then_some(Default::default()),
    };

    let tokens = quote!(
        #signal_struct_def

        #config_struct_def

        impl ::core::convert::AsRef<#signal_ty> for #signal_ty {
            fn as_ref(&self) -> &Self {
                self
            }
        }

        impl ::core::convert::AsMut<#signal_ty> for #signal_ty {
            fn as_mut(&mut self) -> &mut Self {
                self
            }
        }

        impl #leptos_form_krate::DefaultHtmlElement for #ident {
            type El = #leptos_krate::View;
        }

        impl #leptos_form_krate::FormField<#leptos_krate::View> for #ident {
            type Config = #config_ty;
            type Signal = #signal_ty;

            fn default_signal(config: &Self::Config, initial: Option<Self>) -> Self::Signal {
                match initial {
                    Some(initial) => #signal_ty {
                        #(#field_axs: <#field_tys as #leptos_form_krate::FormField<#field_el_tys>>::default_signal(&config.#field_axs, Some(initial.#field_axs)) ,)*
                    },
                    None => #signal_ty {
                        #(#field_axs: <#field_tys as #leptos_form_krate::FormField<#field_el_tys>>::default_signal(&config.#field_axs, None) ,)*
                    },
                }
            }
            fn is_initial_value(signal: &Self::Signal) -> bool {
                true #(&&
                    <#field_tys as #leptos_form_krate::FormField<#field_el_tys>>::is_initial_value(&signal.#field_axs)
                )*
            }
            fn into_signal(self, #config_var_ident: &Self::Config, initial: Option<Self>) -> Self::Signal {
                match initial {
                    Some(initial) => #signal_ty {
                        #(#field_axs: <#field_tys as #leptos_form_krate::FormField<#field_el_tys>>::into_signal(self.#field_axs, &#config_var_ident.#field_axs, Some(initial.#field_axs)) ,)*
                    },
                    None => #signal_ty {
                        #(#field_axs: <#field_tys as #leptos_form_krate::FormField<#field_el_tys>>::into_signal(self.#field_axs, &#config_var_ident.#field_axs, None) ,)*
                    },
                }
            }
            fn try_from_signal(signal: Self::Signal, #config_var_ident: &Self::Config) -> Result<Self, #leptos_form_krate::FormError> {
                Ok(#ident {
                    #(#field_axs: <#field_tys as #leptos_form_krate::FormField<#field_el_tys>>::try_from_signal(signal.#field_axs, &#config_var_ident.#field_axs)? ,)*
                })
            }
            fn recurse(signal: &Self::Signal) {
                #(<#field_tys as #leptos_form_krate::FormField<#field_el_tys>>::recurse(&signal.#field_axs);)*
            }
            fn reset_initial_value(signal: &Self::Signal) {
                #(<#field_tys as #leptos_form_krate::FormField<#field_el_tys>>::reset_initial_value(&signal.#field_axs);)*
            }
        }

        impl #leptos_form_krate::FormComponent<#leptos_krate::View> for #ident {
            #[allow(unused_imports)]
            fn render(#props_ident: #leptos_form_krate::RenderProps<Self::Signal, Self::Config>) -> impl #leptos_krate::IntoView {
                use #leptos_form_krate::FormField;
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
        .map(|x| x.0)
        .unwrap_or_else(|| parse2(quote!(<#ty as #leptos_form_krate::DefaultHtmlElement>::El)).unwrap())
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
        (EH::Default, EH::Default) => quote!(#leptos_krate::view! { <span style="color: red;">{#error_ident}</span> }),
        (EH::Component(component), EH::Default) => quote!(#leptos_krate::view! { <#component error=#error_ident /> }),
        (EH::Container(Container { tag, id, class, style }), EH::Default) => {
            let id = id.iter();
            let class = class.iter();
            let style = style.iter();
            quote!(#leptos_krate::view! { <#tag #(id=#id)* #(class=#class)* #(style=#style)*>{#error_ident}</#tag> })
        }
        (EH::Raw, EH::Default) => quote!({#error_ident}),
        (_, EH::None) => quote!(#leptos_krate::View::default()),
        (_, EH::Component(component)) => quote!(#leptos_krate::view! { <#component error=#error_ident /> }),
        (_, EH::Container(Container { tag, id, class, style })) => {
            let id = id.iter();
            let class = class.iter();
            let style = style.iter();
            quote!(#leptos_krate::view! { <#tag #(id=#id)* #(class=#class)* #(style=#style)*>{#error_ident}</#tag> })
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
    let field_label = field.label.as_ref().unwrap_or(&FieldLabel::Default);
    let (container, id, class, rename_all, style, value) = match (field_label, form_label) {
        (FieldLabel::None, _) => return Ok(quote!(#field_view #error_view)),
        (FieldLabel::Default, FormLabel::None) => return Ok(quote!(#field_view #error_view)),
        (FieldLabel::Default, FormLabel::Default) => (None, None, None, None, None, None),
        (
            FieldLabel::Default,
            FormLabel::Adjacent {
                container,
                id,
                class,
                rename_all,
                style,
            },
        ) => (
            Some(container.clone()),
            id.as_ref(),
            class.as_ref(),
            rename_all.as_ref(),
            style.as_ref(),
            None,
        ),
        (
            FieldLabel::Default,
            FormLabel::Wrap {
                id,
                class,
                rename_all,
                style,
            },
        ) => (
            None,
            id.as_ref(),
            class.as_ref(),
            rename_all.as_ref(),
            style.as_ref(),
            None,
        ),
        (
            FieldLabel::Adjacent {
                container,
                id,
                class,
                style,
                value,
            },
            FormLabel::Adjacent {
                container: container_form,
                id: id_form,
                class: class_form,
                rename_all,
                style: style_form,
            },
        ) => (
            FieldLabelContainer::merge(container.as_ref(), Some(container_form))?,
            id.as_ref().or(id_form.as_ref()),
            class.as_ref().or(class_form.as_ref()),
            if value.is_none() { rename_all.as_ref() } else { None },
            style.as_ref().or(style_form.as_ref()),
            value.as_ref(),
        ),
        (
            FieldLabel::Adjacent {
                container: field_label_container,
                id,
                class,
                style,
                value,
            },
            FormLabel::Wrap {
                id: id_form,
                class: class_form,
                rename_all,
                style: style_form,
            },
        ) => (
            field_label_container.clone().map(Container::try_from).transpose()?,
            id.as_ref().or(id_form.as_ref()),
            class.as_ref().or(class_form.as_ref()),
            if value.is_none() { rename_all.as_ref() } else { None },
            style.as_ref().or(style_form.as_ref()),
            value.as_ref(),
        ),
        (
            FieldLabel::Adjacent {
                container: field_label_container,
                id,
                class,
                style,
                value,
            },
            FormLabel::Default,
        )
        | (
            FieldLabel::Adjacent {
                container: field_label_container,
                id,
                class,
                style,
                value,
            },
            FormLabel::None,
        ) => (
            field_label_container.clone().map(Container::try_from).transpose()?,
            id.as_ref(),
            class.as_ref(),
            None,
            style.as_ref(),
            value.as_ref(),
        ),
        (
            FieldLabel::Wrap {
                id,
                class,
                style,
                value,
            },
            FormLabel::Adjacent {
                id: id_form,
                class: class_form,
                rename_all,
                style: style_form,
                ..
            }
            | FormLabel::Wrap {
                id: id_form,
                class: class_form,
                style: style_form,
                rename_all,
            },
        ) => (
            None,
            id.as_ref().or(id_form.as_ref()),
            class.as_ref().or(class_form.as_ref()),
            if value.is_none() { rename_all.as_ref() } else { None },
            style.as_ref().or(style_form.as_ref()),
            value.as_ref(),
        ),
        (
            FieldLabel::Wrap {
                id,
                class,
                style,
                value,
            },
            FormLabel::Default,
        )
        | (
            FieldLabel::Wrap {
                id,
                class,
                style,
                value,
            },
            FormLabel::None,
        ) => (None, id.as_ref(), class.as_ref(), None, style.as_ref(), value.as_ref()),
    };

    let label_id = id.into_iter();
    let label_class = class.into_iter();
    let label_style = style.into_iter();

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
            let container_style = container.style.as_ref().into_iter();
            quote!(
                <#tag #(id=#container_id)* #(class=#container_class)* #(style=#container_style)*>
                    <label for={#field_id_ident} #(id=#label_id)* #(class=#label_class)* #(style=#label_style)*>
                        #label
                    </label>
                    #field_view
                    #error_view
                </#tag>
            )
        }
        None => quote!(
            <label for={#field_id_ident} #(id=#label_id)* #(class=#label_class)* #(style=#label_style)*>
                <div>#label</div>
                #field_view
                #error_view
            </label>
        ),
    })
}

impl FromMeta for StringExpr {
    fn from_expr(expr: &syn::Expr) -> Result<Self, darling::Error> {
        Ok(match expr {
            syn::Expr::Lit(syn::ExprLit {
                lit: syn::Lit::Str(lit_str),
                ..
            }) => Self::LitStr(lit_str.value()),
            _ => Self::Expr(expr.clone()),
        })
    }
}

impl ToTokens for StringExpr {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            Self::Expr(expr) => tokens.extend(std::iter::once(quote!(#expr))),
            Self::LitStr(lit_str) => tokens.extend(std::iter::once(quote!(#lit_str))),
        }
    }
}

impl StringExpr {
    fn map_case(self, case: Case) -> Self {
        match self {
            Self::Expr(expr) => Self::Expr(expr),
            Self::LitStr(lit_str) => Self::LitStr(lit_str.to_case(case)),
        }
    }

    fn with_oco(leptos_krate: &syn::Path) -> impl Fn(StringExpr) -> TokenStream + '_ {
        move |val| match val {
            Self::Expr(expr) => quote!(#leptos_krate::Oco::Owned(#expr.to_string())),
            Self::LitStr(lit_str) => quote!(#leptos_krate::Oco::Borrowed(#lit_str)),
        }
    }
}

impl ComponentConfigSpanned {
    fn span(&self) -> Span {
        self.field.span()
    }
}

impl FromMeta for ComponentConfigSpanned {
    fn from_meta(meta: &syn::Meta) -> Result<Self, darling::Error> {
        let field = meta
            .path()
            .segments
            .last()
            .ok_or_else(|| darling::Error::custom("no path segment found").with_span(&meta.span()))?
            .ident
            .clone();

        let spanned = match meta {
            syn::Meta::Path(_) => Default::default(),
            syn::Meta::List(_) => ComponentConfig::from_meta(meta)?,
            syn::Meta::NameValue(_) => {
                return Err(darling::Error::custom("unexpected name/value meta attribute").with_span(&meta.span()))
            }
        };

        Ok(Self { field, spanned })
    }
}

impl FromMeta for Action {
    fn from_expr(expr: &syn::Expr) -> Result<Self, darling::Error> {
        use darling::Error;

        let parse_fn_call = |expr_call: &syn::ExprCall| -> Result<(syn::Path, syn::Ident), Error> {
            let server_fn_path = match &*expr_call.func {
                syn::Expr::Path(expr_path) => expr_path.path.clone(),
                _ => {
                    return Err(
                        Error::custom("action server function must use a path (i.e. cannot be a closure)")
                            .with_span(&expr_call.func.span()),
                    )
                }
            };
            let first_arg = expr_call.args.first().ok_or_else(|| {
                Error::custom("expected an argument to the function call").with_span(&expr_call.args.span())
            })?;
            if expr_call.args.len() > 1 {
                return Err(Error::custom("expected a function call with exactly one argument")
                    .with_span(&expr_call.args.span()));
            }
            let arg = match first_arg {
                syn::Expr::Path(expr_path) => expr_path
                    .path
                    .get_ident()
                    .ok_or_else(|| Error::custom("only idents are supported here").with_span(&expr_path.span()))?
                    .clone(),
                _ => return Err(Error::custom("only idents are supported here").with_span(&first_arg.span())),
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
            syn::Expr::Lit(syn::ExprLit {
                lit: syn::Lit::Str(lit_str),
                ..
            }) => Self::Url(lit_str.clone()),
            syn::Expr::Tuple(expr_tuple) => {
                if expr_tuple.elems.len() != 2 {
                    return Err(Error::custom(r#"ActionForm cannot be used without full specification like so: `action = (server_fn_path(data), "/api/url")`"#).with_span(&expr_tuple.elems.span()));
                }
                let first = expr_tuple.elems.first().unwrap();
                let second = expr_tuple.elems.last().unwrap();

                let (server_fn_path, arg) = if let syn::Expr::Call(expr_call) = first {
                    parse_fn_call(expr_call)
                } else {
                    Err(Error::custom("expected a function call expression").with_span(&first.span()))
                }?;

                let url = if let syn::Expr::Lit(syn::ExprLit {
                    lit: syn::Lit::Str(lit_str),
                    ..
                }) = second
                {
                    Ok(lit_str.clone())
                } else {
                    Err(Error::custom("expected an endpoint string literal").with_span(&first.span()))
                }?;

                Self::Path {
                    server_fn_path,
                    arg,
                    url: Some(url),
                }
            }
            _ => {
                return Err(Error::custom(
                    "action must be specified with an endpoint string literal or a server fn call expression",
                ))
            }
        })
    }
}

impl FromMeta for Element {
    fn from_meta(meta: &syn::Meta) -> Result<Self, darling::Error> {
        let ty: syn::Type = match &meta {
            syn::Meta::List(meta_list) => parse2(meta_list.tokens.clone()).map_err(|_| {
                darling::Error::custom("expected valid rust type".to_string()).with_span(&meta_list.tokens.span())
            })?,
            syn::Meta::Path(_) => {
                return Err(darling::Error::custom(
                    "el type unspecified, use `el(leptos::html::HtmlElement<..>)`",
                ))
            }
            syn::Meta::NameValue(_) => {
                return Err(darling::Error::custom(
                    "el must be specified using parentheses like `el(leptos::html::HtmlElement<..>)`",
                )
                .with_span(&meta.span()))
            }
        };
        Ok(Self(ty))
    }
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

impl FromMeta for MapSubmit {
    fn from_expr(expr: &syn::Expr) -> Result<Self, darling::Error> {
        use darling::Error;
        Ok(match expr {
            syn::Expr::Path(expr_path) => Self::Path(expr_path.path.clone()),
            syn::Expr::Closure(expr_closure) => Self::Defn(expr_closure.clone()),
            _ => return Err(Error::custom("expected a path or a closure").with_span(&expr.span())),
        })
    }
}

impl FromMeta for CacheValue {
    fn from_meta(meta: &syn::Meta) -> Result<Self, darling::Error> {
        let expr: syn::Expr = match &meta {
            syn::Meta::List(meta_list) => parse2(meta_list.tokens.clone()).map_err(|_| {
                darling::Error::custom("expected valid rust type".to_string()).with_span(&meta_list.tokens.span())
            })?,
            syn::Meta::Path(_) => return Err(darling::Error::custom("ty must be specified, use `ty(..)`")),
            syn::Meta::NameValue(_) => {
                return Err(darling::Error::custom(
                    "ty must be specified using parentheses like `ty(leptos::html::HtmlElement<..>)`",
                )
                .with_span(&meta.span()))
            }
        };
        Ok(Self(expr))
    }
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

impl From<Container> for FieldLabelContainer {
    fn from(value: Container) -> Self {
        Self {
            tag: Some(value.tag),
            id: value.id,
            class: value.class,
            style: value.style,
        }
    }
}

impl TryFrom<FieldLabelContainer> for Container {
    type Error = Error;
    fn try_from(value: FieldLabelContainer) -> Result<Self, Self::Error> {
        Ok(Self {
            tag: value
                .tag
                .ok_or_else(|| Error::new(Span::call_site(), "field label container must specify tag"))?,
            id: value.id,
            class: value.class,
            style: value.style,
        })
    }
}

impl FieldLabelContainer {
    fn merge(field: Option<&Self>, form: Option<&Container>) -> Result<Option<Container>, Error> {
        Ok(match (field.cloned(), form.cloned()) {
            (Some(field), Some(form)) => Some(Container {
                tag: field.tag.unwrap_or(form.tag),
                id: field.id.or(form.id),
                class: field.class.or(form.class),
                style: field.style.or(form.style),
            }),
            (Some(field), None) => Some(Container::try_from(field)?),
            (None, Some(form)) => Some(form),
            (None, None) => None,
        })
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use pretty_assertions::assert_eq;

    fn expect_err(res: Result<TokenStream, Error>) -> Error {
        match res {
            Ok(tokens) => panic!("expected failure but derive macro successfully ran with tokens:\n{tokens}"),
            Err(err) => err,
        }
    }

    fn cleanup(tokens: &TokenStream) -> String {
        let tokens = tokens.to_string();
        tokens.replace("< <", "<<").replace("> >", ">>")
    }

    fn pretty(cleaned_up: String) -> Result<String, Error> {
        let syntax_tree = syn::parse_file(&cleaned_up)?;
        Ok(prettyplease::unparse(&syntax_tree))
    }

    #[test]
    fn form_cannot_be_derived_on_unit_structs() {
        let input = quote!(
            #[derive(Form)]
            #[form(component)]
            pub struct MyFormData;
        );

        let err = expect_err(derive_form(input));

        assert_eq!(
            "Unsupported shape `no fields`. Expected named fields or unnamed fields.",
            format!("{err}")
        );
    }

    #[test]
    fn component_attribute_must_provide_arguments() {
        let input = quote!(
            #[derive(Form)]
            #[form(component)]
            pub struct MyFormData {
                pub id: Uuid,
            }
        );

        let err = expect_err(derive_form(input));

        assert_eq!("component forms must specify an action attribute", format!("{err}"));
    }

    #[test]
    fn component_attribute_must_not_use_name_value_meta() {
        let input = quote!(
            #[derive(Form)]
            #[form(component = "/api/test")]
            pub struct MyFormData {
                pub id: Uuid,
            }
        );

        let err = expect_err(derive_form(input));

        assert_eq!("unexpected name/value meta attribute", format!("{err}"));
    }

    #[test]
    fn component_and_island_attributes_cannot_be_used_simultaneously() {
        let input = quote!(
            #[derive(Form)]
            #[form(component(action = "/api/test"), island(action = "/api/test"))]
            pub struct MyFormData {
                pub id: Uuid,
            }
        );

        let err = expect_err(derive_form(input));

        assert_eq!("cannot set component and island", format!("{err}"));
    }

    #[test]
    fn action_server_fn_must_be_a_valid_expr_fn_call() {
        // test that action function specification does not accept paths
        let input = quote!(
            #[derive(Form)]
            #[form(component(action = my_action))]
            pub struct MyFormData {
                pub id: Uuid,
            }
        );

        let err = expect_err(derive_form(input));

        assert_eq!(
            "action must be specified with an endpoint string literal or a server fn call expression",
            format!("{err}")
        );

        // test that action function call expressions do not work without arguments
        let input = quote!(
            #[derive(Form)]
            #[form(component(action = my_action()))]
            pub struct MyFormData {
                pub id: Uuid,
            }
        );

        let err = expect_err(derive_form(input));

        assert_eq!("expected an argument to the function call", format!("{err}"));

        // test that action function call expressions do not work with more than one argument
        let input = quote!(
            #[derive(Form)]
            #[form(component(action = my_action(too, many, args)))]
            pub struct MyFormData {
                pub id: Uuid,
            }
        );

        let err = expect_err(derive_form(input));

        assert_eq!("expected a function call with exactly one argument", format!("{err}"));

        // test that action function call expressions do not work when the argument provided is a path and not an ident
        let input = quote!(
            #[derive(Form)]
            #[form(component(action = my_action(not::an::ident)))]
            pub struct MyFormData {
                pub id: Uuid,
            }
        );

        let err = expect_err(derive_form(input));

        assert_eq!("only idents are supported here", format!("{err}"));

        // test that action function call expressions do not work when the argument provided is a literal and not an ident
        let input = quote!(
            #[derive(Form)]
            #[form(component(action = my_action(1)))]
            pub struct MyFormData {
                pub id: Uuid,
            }
        );

        let err = expect_err(derive_form(input));

        assert_eq!("only idents are supported here", format!("{err}"));

        // test that the func in an action function must be a syn::Path
        let input = quote!(
            #[derive(Form)]
            #[form(component(action = (|data| data)(my_data)))]
            pub struct MyFormData {
                pub id: Uuid,
            }
        );

        let err = expect_err(derive_form(input));

        assert_eq!(
            "action server function must use a path (i.e. cannot be a closure)",
            format!("{err}")
        );
    }

    #[test]
    fn action_server_fn_accepts_valid_expr_fn_call() -> Result<(), Error> {
        let input = quote!(
            #[derive(Form)]
            #[form(component(action = my_action(my_form_data)))]
            pub struct MyFormData {
                pub id: Uuid,
            }
        );

        derive_form(input)?;

        Ok(())
    }

    #[test]
    fn action_server_fn_rejects_invalid_tuples() {
        // missing tuple elements should be rejected
        let input = quote!(
            #[derive(Form)]
            #[form(component(action = ()))]
            pub struct MyFormData {
                pub id: Uuid,
            }
        );

        let err = expect_err(derive_form(input));

        assert_eq!(
            r#"ActionForm cannot be used without full specification like so: `action = (server_fn_path(data), "/api/url")`"#,
            format!("{err}")
        );

        // missing tuple elements should be rejected
        let input = quote!(
            #[derive(Form)]
            #[form(component(action = ("/api/test",)))]
            pub struct MyFormData {
                pub id: Uuid,
            }
        );

        let err = expect_err(derive_form(input));
        assert_eq!(
            r#"ActionForm cannot be used without full specification like so: `action = (server_fn_path(data), "/api/url")`"#,
            format!("{err}")
        );

        // tuple ordering should be (fn call, endpoint literal)
        let input = quote!(
            #[derive(Form)]
            #[form(component(action = ("/api/test", my_action(my_form_data))))]
            pub struct MyFormData {
                pub id: Uuid,
            }
        );

        let err = expect_err(derive_form(input));
        assert_eq!("expected a function call expression", format!("{err}"));

        // enforce string literal parsing on second argument
        let input = quote!(
            #[derive(Form)]
            #[form(component(action = (my_action(my_form_data), my_action(my_form_data))))]
            pub struct MyFormData {
                pub id: Uuid,
            }
        );

        let err = expect_err(derive_form(input));
        assert_eq!("expected an endpoint string literal", format!("{err}"));
    }

    #[test]
    fn action_server_fn_accepts_valid_tuple_of_expr_fn_call_and_endpoint_path() -> Result<(), Error> {
        let input = quote!(
            #[derive(Form)]
            #[form(component(action = (my_action(my_form_data), "/api/test")))]
            pub struct MyFormData {
                pub id: Uuid,
            }
        );

        derive_form(input)?;

        Ok(())
    }

    #[test]
    fn field_element_rejects_path_meta() {
        let input = quote!(
            #[derive(Form)]
            pub struct MyFormData {
                #[form(el)]
                pub id: Uuid,
            }
        );

        let err = expect_err(derive_form(input));

        assert_eq!(
            "el type unspecified, use `el(leptos::html::HtmlElement<..>)`",
            format!("{err}")
        );
    }

    #[test]
    fn field_element_rejects_name_value_meta() {
        let input = quote!(
            #[derive(Form)]
            pub struct MyFormData {
                #[form(el = MyElement)]
                pub id: Uuid,
            }
        );

        let err = expect_err(derive_form(input));

        assert_eq!(
            "el must be specified using parentheses like `el(leptos::html::HtmlElement<..>)`",
            format!("{err}")
        );
    }

    #[test]
    fn field_element_rejects_non_types() {
        let input = quote!(
            #[derive(Form)]
            pub struct MyFormData {
                #[form(el(1+1))]
                pub id: Uuid,
            }
        );

        let err = expect_err(derive_form(input));

        // less than ideal error message but cannot control that darling attempts to parse the `el = TokenStream` and fails
        // prior to calling from_meta
        assert_eq!("expected valid rust type", format!("{err}"));
    }

    #[test]
    fn field_element_accepts_valid_types_when_parenthesized() -> Result<(), Error> {
        let input = quote!(
            #[derive(Form)]
            pub struct MyFormData {
                #[form(el(::leptos::HtmlElement<::leptos::html::Input>))]
                pub id: Uuid,
            }
        );

        derive_form(input)?;

        Ok(())
    }

    #[test]
    fn group_item_rejects_non_container() {
        let input = quote!(
            #[derive(Form)]
            #[form(groups(1))]
            pub struct MyFormData {
                pub id: Uuid,
            }
        );

        let err = expect_err(derive_form(input));

        assert_eq!("expected argument of the form `container(..)`", format!("{err}"));
    }

    #[test]
    fn group_item_accepts_container() -> Result<(), Error> {
        let input = quote!(
            #[derive(Form)]
            #[form(groups(container(tag = "div")))]
            pub struct MyFormData {
                pub id: Uuid,
            }
        );

        derive_form(input)?;

        Ok(())
    }

    #[test]
    fn field_group_out_of_range_is_rejected() {
        let input = quote!(
            #[derive(Form)]
            #[form(groups(container(tag = "div")))]
            pub struct MyFormData {
                #[form(group = 0)]
                pub id: Uuid,
                #[form(group = 2)]
                pub name: Uuid,
            }
        );

        let err = expect_err(derive_form(input));

        assert_eq!("group index out of range", format!("{err}"));
    }

    #[test]
    fn tuple_component_unnamed_is_rejected() {
        let expected_err_msg = r#"deriving Form for a tuple struct with `component` specified additionally requires a component name, consider using `component(name = "MyFormData_")`; note that "MyFormData" cannot be used as the component name because MyFormData is already an item defined in the function namespace and "_MyFormData" should not be used because of leptos name transformations"#;

        let input = quote!(
            #[derive(Form)]
            #[form(component(action = "foo"))]
            pub struct MyFormData(Uuid);
        );

        let err = expect_err(derive_form(input));

        assert_eq!(expected_err_msg, format!("{err}"));
    }

    #[test]
    fn tuple_component_same_name_is_rejected() {
        let expected_err_msg = r#"deriving Form for a tuple struct with `component` specified additionally requires a component name, consider using `component(name = "MyFormData_")`; note that "MyFormData" cannot be used as the component name because MyFormData is already an item defined in the function namespace and "_MyFormData" should not be used because of leptos name transformations"#;

        let input = quote!(
            #[derive(Form)]
            #[form(component(action = "foo", name = MyFormData))]
            pub struct MyFormData(Uuid);
        );

        let err = expect_err(derive_form(input));

        assert_eq!(expected_err_msg, format!("{err}"));
    }

    #[test]
    fn tuple_component_same_name_preceded_with_underscore_is_rejected() {
        let expected_err_msg = r#"deriving Form for a tuple struct with `component` specified additionally requires a component name, consider using `component(name = "MyFormData_")`; note that "MyFormData" cannot be used as the component name because MyFormData is already an item defined in the function namespace and "_MyFormData" should not be used because of leptos name transformations"#;

        let input = quote!(
            #[derive(Form)]
            #[form(component(action = "foo", name = _MyFormData))]
            pub struct MyFormData(Uuid);
        );

        let err = expect_err(derive_form(input));

        assert_eq!(expected_err_msg, format!("{err}"));
    }

    #[test]
    fn tuple_component_name_valid() -> Result<(), Error> {
        let input = quote!(
            #[derive(Form)]
            #[form(component(action = "foo", name = Test))]
            pub struct MyFormData(Uuid);
        );

        derive_form(input)?;

        Ok(())
    }

    #[test]
    fn map_submit_rejects_non_path_or_closure() {
        let input = quote!(
            #[derive(Form)]
            #[form(component(
                action = "foo",
                map_submit = 1 + 1,
            ))]
            pub struct MyFormData {
                pub id: Uuid,
            }
        );

        let err = expect_err(derive_form(input));

        assert_eq!("expected a path or a closure", format!("{err}"));
    }

    #[test]
    fn map_submit_accepts_path() -> Result<(), Error> {
        let input = quote!(
            #[derive(Form)]
            #[form(component(
                action = "foo",
                map_submit = map_my_submit,
            ))]
            pub struct MyFormData {
                pub id: Uuid,
            }
        );

        derive_form(input)?;

        Ok(())
    }

    #[test]
    fn basic_form_comparison_check() -> Result<(), Error> {
        let input = quote!(
            #[derive(Form)]
            #[form(label(wrap(rename_all = "Title Case")))]
            pub struct MyFormData {
                pub id: Uuid,
                pub slug: String,
                pub created_at: chrono::NaiveDateTime,
                pub count: u8,
            }
        );

        let leptos_form_krate = quote!(::leptos_form);
        let leptos_krate = quote!(#leptos_form_krate::internal::leptos);
        let _wasm_bindgen_krate = quote!(#leptos_form_krate::internal::wasm_bindgen);

        let expected = quote!(
            #[derive(Clone, Copy, Debug)]
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
                fn default_signal(config: &Self::Config, initial: Option<Self>) -> Self::Signal {
                    match initial {
                        Some(initial) => {
                            __MyFormDataSignal {
                                id: <Uuid as ::leptos_form::FormField<
                                    <Uuid as ::leptos_form::DefaultHtmlElement>::El,
                                >>::default_signal(&config.id, Some(initial.id)),
                                slug: <String as ::leptos_form::FormField<
                                    <String as ::leptos_form::DefaultHtmlElement>::El,
                                >>::default_signal(&config.slug, Some(initial.slug)),
                                created_at: <chrono::NaiveDateTime as ::leptos_form::FormField<
                                    <chrono::NaiveDateTime as ::leptos_form::DefaultHtmlElement>::El,
                                >>::default_signal(&config.created_at, Some(initial.created_at)),
                                count: <u8 as ::leptos_form::FormField<
                                    <u8 as ::leptos_form::DefaultHtmlElement>::El,
                                >>::default_signal(&config.count, Some(initial.count)),
                            }
                        }
                        None => {
                            __MyFormDataSignal {
                                id: <Uuid as ::leptos_form::FormField<
                                    <Uuid as ::leptos_form::DefaultHtmlElement>::El,
                                >>::default_signal(&config.id, None),
                                slug: <String as ::leptos_form::FormField<
                                    <String as ::leptos_form::DefaultHtmlElement>::El,
                                >>::default_signal(&config.slug, None),
                                created_at: <chrono::NaiveDateTime as ::leptos_form::FormField<
                                    <chrono::NaiveDateTime as ::leptos_form::DefaultHtmlElement>::El,
                                >>::default_signal(&config.created_at, None),
                                count: <u8 as ::leptos_form::FormField<
                                    <u8 as ::leptos_form::DefaultHtmlElement>::El,
                                >>::default_signal(&config.count, None),
                            }
                        }
                    }
                }
                fn is_initial_value(signal: &Self::Signal) -> bool {
                    true && <Uuid as #leptos_form_krate::FormField<<Uuid as #leptos_form_krate::DefaultHtmlElement>::El>>::is_initial_value(&signal.id) &&
                    <String as #leptos_form_krate::FormField<<String as #leptos_form_krate::DefaultHtmlElement>::El>>::is_initial_value(&signal.slug) &&
                    <chrono::NaiveDateTime as #leptos_form_krate::FormField<<chrono::NaiveDateTime as #leptos_form_krate::DefaultHtmlElement>::El>>::is_initial_value(&signal.created_at) &&
                    <u8 as #leptos_form_krate::FormField<<u8 as #leptos_form_krate::DefaultHtmlElement>::El>>::is_initial_value(&signal.count)
                }
                fn into_signal(self, config: &Self::Config, initial: Option<Self>) -> Self::Signal {
                    match initial {
                        Some(initial) => {
                            __MyFormDataSignal {
                                id: <Uuid as ::leptos_form::FormField<
                                    <Uuid as ::leptos_form::DefaultHtmlElement>::El,
                                >>::into_signal(self.id, &config.id, Some(initial.id)),
                                slug: <String as ::leptos_form::FormField<
                                    <String as ::leptos_form::DefaultHtmlElement>::El,
                                >>::into_signal(self.slug, &config.slug, Some(initial.slug)),
                                created_at: <chrono::NaiveDateTime as ::leptos_form::FormField<
                                    <chrono::NaiveDateTime as ::leptos_form::DefaultHtmlElement>::El,
                                >>::into_signal(
                                    self.created_at,
                                    &config.created_at,
                                    Some(initial.created_at),
                                ),
                                count: <u8 as ::leptos_form::FormField<
                                    <u8 as ::leptos_form::DefaultHtmlElement>::El,
                                >>::into_signal(self.count, &config.count, Some(initial.count)),
                            }
                        }
                        None => {
                            __MyFormDataSignal {
                                id: <Uuid as ::leptos_form::FormField<
                                    <Uuid as ::leptos_form::DefaultHtmlElement>::El,
                                >>::into_signal(self.id, &config.id, None),
                                slug: <String as ::leptos_form::FormField<
                                    <String as ::leptos_form::DefaultHtmlElement>::El,
                                >>::into_signal(self.slug, &config.slug, None),
                                created_at: <chrono::NaiveDateTime as ::leptos_form::FormField<
                                    <chrono::NaiveDateTime as ::leptos_form::DefaultHtmlElement>::El,
                                >>::into_signal(self.created_at, &config.created_at, None),
                                count: <u8 as ::leptos_form::FormField<
                                    <u8 as ::leptos_form::DefaultHtmlElement>::El,
                                >>::into_signal(self.count, &config.count, None),
                            }
                        }
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
                fn recurse(signal: &Self::Signal) {
                    <Uuid as ::leptos_form::FormField<
                        <Uuid as ::leptos_form::DefaultHtmlElement>::El,
                    >>::recurse(&signal.id);
                    <String as ::leptos_form::FormField<
                        <String as ::leptos_form::DefaultHtmlElement>::El,
                    >>::recurse(&signal.slug);
                    <chrono::NaiveDateTime as ::leptos_form::FormField<
                        <chrono::NaiveDateTime as ::leptos_form::DefaultHtmlElement>::El,
                    >>::recurse(&signal.created_at);
                    <u8 as ::leptos_form::FormField<
                        <u8 as ::leptos_form::DefaultHtmlElement>::El,
                    >>::recurse(&signal.count);
                }
                fn reset_initial_value(signal: &Self::Signal) {
                    <Uuid as #leptos_form_krate::FormField<<Uuid as #leptos_form_krate::DefaultHtmlElement>::El>>::reset_initial_value(&signal.id);
                    <String as #leptos_form_krate::FormField<<String as #leptos_form_krate::DefaultHtmlElement>::El>>::reset_initial_value(&signal.slug);
                    <chrono::NaiveDateTime as #leptos_form_krate::FormField<<chrono::NaiveDateTime as #leptos_form_krate::DefaultHtmlElement>::El>>::reset_initial_value(&signal.created_at);
                    <u8 as #leptos_form_krate::FormField<<u8 as #leptos_form_krate::DefaultHtmlElement>::El>>::reset_initial_value(&signal.count);
                }
            }

            impl #leptos_form_krate::FormComponent<#leptos_krate::View> for MyFormData {
                #[allow(unused_imports)]
                fn render(props: #leptos_form_krate::RenderProps<Self::Signal, Self::Config>) -> impl #leptos_krate::IntoView {
                    use #leptos_form_krate::FormField;
                    use #leptos_krate::{IntoAttribute, IntoView};

                    let _id_id = #leptos_form_krate::format_form_id(props.id.as_ref(), #leptos_krate::Oco::Borrowed("id"));
                    let _id_name = #leptos_form_krate::format_form_name(props.name.as_ref(), "id");
                    let _id_props = #leptos_form_krate::RenderProps::builder()
                        .id(_id_id.clone())
                        .name(_id_name.clone())
                        .field_changed_class(props.field_changed_class.clone())
                        .signal(props.signal.id.clone())
                        .config(<Uuid as #leptos_form_krate::FormField<<Uuid as #leptos_form_krate::DefaultHtmlElement>::El>>::Config::default())
                        .build();

                    let _id_error = move || <Uuid as #leptos_form_krate::FormField<<Uuid as #leptos_form_krate::DefaultHtmlElement>::El>>::with_error(
                        &_id_props.signal,
                        |error| match error {
                            Some(form_error) => {
                                let error = format!("{form_error}");
                                #leptos_krate::IntoView::into_view(#leptos_krate::view! { <span style="color: red;">{error}</span> })
                            },
                            None => #leptos_krate::View::default(),
                        },
                    );
                    let ty = <::std::marker::PhantomData<(Uuid, <Uuid as #leptos_form_krate::DefaultHtmlElement>::El)> as Default>::default();
                    let _id_view = #leptos_krate::view! { <FormField props=_id_props ty=ty /> };

                    let _slug_id = #leptos_form_krate::format_form_id(props.id.as_ref(), #leptos_krate::Oco::Borrowed("slug"));
                    let _slug_name = #leptos_form_krate::format_form_name(props.name.as_ref(), "slug");
                    let _slug_props = #leptos_form_krate::RenderProps::builder()
                        .id(_slug_id.clone())
                        .name(_slug_name.clone())
                        .field_changed_class(props.field_changed_class.clone())
                        .signal(props.signal.slug.clone())
                        .config(<String as #leptos_form_krate::FormField<<String as #leptos_form_krate::DefaultHtmlElement>::El>>::Config::default())
                        .build();

                    let _slug_error = move || <String as #leptos_form_krate::FormField<<String as #leptos_form_krate::DefaultHtmlElement>::El>>::with_error(
                        &_slug_props.signal,
                        |error| match error {
                            Some(form_error) => {
                                let error = format!("{form_error}");
                                #leptos_krate::IntoView::into_view(#leptos_krate::view! { <span style="color: red;">{error}</span> })
                            },
                            None => #leptos_krate::View::default(),
                        },
                    );
                    let ty = <::std::marker::PhantomData<(String, <String as #leptos_form_krate::DefaultHtmlElement>::El)> as Default>::default();
                    let _slug_view = #leptos_krate::view! { <FormField props=_slug_props ty=ty /> };

                    let _created_at_id = #leptos_form_krate::format_form_id(props.id.as_ref(), #leptos_krate::Oco::Borrowed("created-at"));
                    let _created_at_name = #leptos_form_krate::format_form_name(props.name.as_ref(), "created_at");
                    let _created_at_props = #leptos_form_krate::RenderProps::builder()
                        .id(_created_at_id.clone())
                        .name(_created_at_name.clone())
                        .field_changed_class(props.field_changed_class.clone())
                        .signal(props.signal.created_at.clone())
                        .config(<chrono::NaiveDateTime as #leptos_form_krate::FormField<<chrono::NaiveDateTime as #leptos_form_krate::DefaultHtmlElement>::El>>::Config::default())
                        .build();

                    let _created_at_error = move || <chrono::NaiveDateTime as #leptos_form_krate::FormField<<chrono::NaiveDateTime as #leptos_form_krate::DefaultHtmlElement>::El>>::with_error(
                        &_created_at_props.signal,
                        |error| match error {
                            Some(form_error) => {
                                let error = format!("{form_error}");
                                #leptos_krate::IntoView::into_view(#leptos_krate::view! { <span style="color: red;">{error}</span> })
                            },
                            None => #leptos_krate::View::default(),
                        },
                    );
                    let ty = <::std::marker::PhantomData<(chrono::NaiveDateTime, <chrono::NaiveDateTime as #leptos_form_krate::DefaultHtmlElement>::El)> as Default>::default();
                    let _created_at_view = #leptos_krate::view! { <FormField props=_created_at_props ty=ty /> };

                    let _count_id = #leptos_form_krate::format_form_id(props.id.as_ref(), #leptos_krate::Oco::Borrowed("count"));
                    let _count_name = #leptos_form_krate::format_form_name(props.name.as_ref(), "count");
                    let _count_props = #leptos_form_krate::RenderProps::builder()
                        .id(_count_id.clone())
                        .name(_count_name.clone())
                        .field_changed_class(props.field_changed_class.clone())
                        .signal(props.signal.count.clone())
                        .config(<u8 as #leptos_form_krate::FormField<<u8 as #leptos_form_krate::DefaultHtmlElement>::El>>::Config::default())
                        .build();

                    let _count_error = move || <u8 as #leptos_form_krate::FormField<<u8 as #leptos_form_krate::DefaultHtmlElement>::El>>::with_error(
                        &_count_props.signal,
                        |error| match error {
                            Some(form_error) => {
                                let error = format!("{form_error}");
                                #leptos_krate::IntoView::into_view(#leptos_krate::view! { <span style="color: red;">{error}</span> })
                            },
                            None => #leptos_krate::View::default(),
                        },
                    );
                    let ty = <::std::marker::PhantomData<(u8, <u8 as #leptos_form_krate::DefaultHtmlElement>::El)> as Default>::default();
                    let _count_view = #leptos_krate::view! { <FormField props=_count_props ty=ty /> };

                    #leptos_krate::view! {
                        <label for={_id_id}>
                            <div>"Id"</div>
                            {_id_view}
                            {_id_error}
                        </label>
                        <label for={_slug_id}>
                            <div>"Slug"</div>
                            {_slug_view}
                            {_slug_error}
                        </label>
                        <label for={_created_at_id}>
                            <div>"Created At"</div>
                            {_created_at_view}
                            {_created_at_error}
                        </label>
                        <label for={_count_id}>
                            <div>"Count"</div>
                            {_count_view}
                            {_count_error}
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
    fn both_form_and_field_level_class_and_labels_work() -> Result<(), Error> {
        let input = quote!(
            #[derive(Form)]
            pub struct MyFormData {
                #[form(class = "hi", id = "hello-there", label(wrap(class = "test", value = "AYO")))]
                pub abc_123: Uuid,
                #[form(label = "none")]
                pub zz: u8,
            }
        );

        let leptos_krate = quote!(::leptos_form::internal::leptos);
        let leptos_form_krate = quote!(::leptos_form);

        let expected = quote!(
            #[derive(Clone, Copy, Debug)]
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

                fn default_signal(config: &Self::Config, initial: Option<Self>) -> Self::Signal {
                    match initial {
                       Some(initial) => {
                           __MyFormDataSignal {
                               abc_123: <Uuid as ::leptos_form::FormField<
                                   <Uuid as ::leptos_form::DefaultHtmlElement>::El,
                               >>::default_signal(&config.abc_123, Some(initial.abc_123)),
                               zz: <u8 as ::leptos_form::FormField<
                                   <u8 as ::leptos_form::DefaultHtmlElement>::El,
                               >>::default_signal(&config.zz, Some(initial.zz)),
                           }
                       }
                       None => {
                           __MyFormDataSignal {
                               abc_123: <Uuid as ::leptos_form::FormField<
                                   <Uuid as ::leptos_form::DefaultHtmlElement>::El,
                               >>::default_signal(&config.abc_123, None),
                               zz: <u8 as ::leptos_form::FormField<
                                   <u8 as ::leptos_form::DefaultHtmlElement>::El,
                               >>::default_signal(&config.zz, None),
                           }
                       }
                    }
                }
                fn is_initial_value(signal: &Self::Signal) -> bool {
                    true && <Uuid as #leptos_form_krate::FormField<<Uuid as #leptos_form_krate::DefaultHtmlElement>::El>>::is_initial_value(&signal.abc_123) &&
                    <u8 as #leptos_form_krate::FormField<<u8 as #leptos_form_krate::DefaultHtmlElement>::El>>::is_initial_value(&signal.zz)
                }
                fn into_signal(self, config: &Self::Config, initial: Option<Self>) -> Self::Signal {
                    match initial {
                        Some(initial) => {
                            __MyFormDataSignal {
                                abc_123: <Uuid as ::leptos_form::FormField<
                                    <Uuid as ::leptos_form::DefaultHtmlElement>::El,
                                >>::into_signal(
                                    self.abc_123,
                                    &config.abc_123,
                                    Some(initial.abc_123),
                                ),
                                zz: <u8 as ::leptos_form::FormField<
                                    <u8 as ::leptos_form::DefaultHtmlElement>::El,
                                >>::into_signal(self.zz, &config.zz, Some(initial.zz)),
                            }
                        }
                        None => {
                            __MyFormDataSignal {
                                abc_123: <Uuid as ::leptos_form::FormField<
                                    <Uuid as ::leptos_form::DefaultHtmlElement>::El,
                                >>::into_signal(self.abc_123, &config.abc_123, None),
                                zz: <u8 as ::leptos_form::FormField<
                                    <u8 as ::leptos_form::DefaultHtmlElement>::El,
                                >>::into_signal(self.zz, &config.zz, None),
                            }
                        }
                    }
                }
                fn try_from_signal(signal: Self::Signal, config: &Self::Config) -> Result<Self, #leptos_form_krate::FormError> {
                    Ok(MyFormData {
                        abc_123: <Uuid as #leptos_form_krate::FormField<<Uuid as #leptos_form_krate::DefaultHtmlElement>::El>>::try_from_signal(signal.abc_123, &config.abc_123)?,
                        zz: <u8 as #leptos_form_krate::FormField<<u8 as #leptos_form_krate::DefaultHtmlElement>::El>>::try_from_signal(signal.zz, &config.zz)?,
                    })
                }
                fn recurse(signal: &Self::Signal) {
                    <Uuid as ::leptos_form::FormField<
                        <Uuid as ::leptos_form::DefaultHtmlElement>::El,
                    >>::recurse(&signal.abc_123);
                    <u8 as ::leptos_form::FormField<
                        <u8 as ::leptos_form::DefaultHtmlElement>::El,
                    >>::recurse(&signal.zz);
                }
                fn reset_initial_value(signal: &Self::Signal) {
                    <Uuid as #leptos_form_krate::FormField<<Uuid as #leptos_form_krate::DefaultHtmlElement>::El>>::reset_initial_value(&signal.abc_123);
                    <u8 as #leptos_form_krate::FormField<<u8 as #leptos_form_krate::DefaultHtmlElement>::El>>::reset_initial_value(&signal.zz);
                }
            }

            impl #leptos_form_krate::FormComponent<#leptos_krate::View> for MyFormData {
                #[allow(unused_imports)]
                fn render(props: #leptos_form_krate::RenderProps<Self::Signal, Self::Config>) -> impl #leptos_krate::IntoView {
                    use #leptos_form_krate::FormField;
                    use #leptos_krate::{IntoAttribute, IntoView};

                    let _abc_123_id = #leptos_form_krate::format_form_id(props.id.as_ref(), #leptos_krate::Oco::Borrowed("hello-there"));
                    let _abc_123_name = #leptos_form_krate::format_form_name(props.name.as_ref(), "abc_123");
                    let _abc_123_props = #leptos_form_krate::RenderProps::builder()
                        .id(_abc_123_id.clone())
                        .name(_abc_123_name.clone())
                        .class(#leptos_krate::Oco::Borrowed("hi"))
                        .field_changed_class(props.field_changed_class.clone())
                        .signal(props.signal.abc_123.clone())
                        .config(<Uuid as #leptos_form_krate::FormField<<Uuid as #leptos_form_krate::DefaultHtmlElement>::El>>::Config::default())
                        .build();

                    let _abc_123_error = move || <Uuid as #leptos_form_krate::FormField<<Uuid as #leptos_form_krate::DefaultHtmlElement>::El>>::with_error(
                        &_abc_123_props.signal,
                        |error| match error {
                            Some(form_error) => {
                                let error = format!("{form_error}");
                                #leptos_krate::IntoView::into_view(#leptos_krate::view! { <span style="color: red;">{error}</span> })
                            },
                            None => #leptos_krate::View::default(),
                        },
                    );
                    let ty = <::std::marker::PhantomData<(Uuid, <Uuid as #leptos_form_krate::DefaultHtmlElement>::El)> as Default>::default();
                    let _abc_123_view = #leptos_krate::view! { <FormField props=_abc_123_props ty=ty /> };

                    let _zz_id = #leptos_form_krate::format_form_id(props.id.as_ref(), #leptos_krate::Oco::Borrowed("zz"));
                    let _zz_name = #leptos_form_krate::format_form_name(props.name.as_ref(), "zz");
                    let _zz_props = #leptos_form_krate::RenderProps::builder()
                        .id(_zz_id.clone())
                        .name(_zz_name.clone())
                        .field_changed_class(props.field_changed_class.clone())
                        .signal(props.signal.zz.clone())
                        .config(<u8 as #leptos_form_krate::FormField<<u8 as #leptos_form_krate::DefaultHtmlElement>::El>>::Config::default())
                        .build();

                    let _zz_error = move || <u8 as #leptos_form_krate::FormField<<u8 as #leptos_form_krate::DefaultHtmlElement>::El>>::with_error(
                        &_zz_props.signal,
                        |error| match error {
                            Some(form_error) => {
                                let error = format!("{form_error}");
                                #leptos_krate::IntoView::into_view(#leptos_krate::view! { <span style="color: red;">{error}</span> })
                            },
                            None => #leptos_krate::View::default(),
                        },
                    );
                    let ty = <::std::marker::PhantomData<(u8, <u8 as #leptos_form_krate::DefaultHtmlElement>::El)> as Default>::default();
                    let _zz_view = #leptos_krate::view! { <FormField props=_zz_props ty=ty /> };

                    #leptos_krate::view! {
                        <label for={_abc_123_id} class="test">
                            <div>"AYO"</div>
                            {_abc_123_view}
                            {_abc_123_error}
                        </label>
                        {_zz_view}
                        {_zz_error}
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
    fn component_is_produced_correctly() -> Result<(), Error> {
        let input = quote!(
            #[derive(Form)]
            #[form(component(action = "/api/my-form-data", reset_on_success))]
            pub struct MyFormData {
                pub ayo: u8,
            }
        );

        let leptos_form_krate = quote!(::leptos_form);
        let leptos_krate = quote!(#leptos_form_krate::internal::leptos);
        let leptos_router_krate = quote!(#leptos_form_krate::internal::leptos_router);

        let expected = quote!(
            #[derive(Clone, Copy, Debug)]
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

                fn default_signal(config: &Self::Config, initial: Option<Self>) -> Self::Signal {
                    match initial {
                        Some(initial) => __MyFormDataSignal {
                            ayo: <u8 as #leptos_form_krate::FormField<<u8 as #leptos_form_krate::DefaultHtmlElement>::El>>::default_signal(&config.ayo, Some(initial.ayo)),
                        },
                        None => __MyFormDataSignal {
                            ayo: <u8 as #leptos_form_krate::FormField<<u8 as #leptos_form_krate::DefaultHtmlElement>::El>>::default_signal(&config.ayo, None),
                        },
                    }
                }
                fn is_initial_value(signal: &Self::Signal) -> bool {
                    true && <u8 as #leptos_form_krate::FormField<<u8 as #leptos_form_krate::DefaultHtmlElement>::El>>::is_initial_value(&signal.ayo)
                }
                fn into_signal(self, config: &Self::Config, initial: Option<Self>) -> Self::Signal {
                    match initial {
                        Some(initial) => {
                            __MyFormDataSignal {
                                ayo: <u8 as ::leptos_form::FormField<
                                    <u8 as ::leptos_form::DefaultHtmlElement>::El,
                                >>::into_signal(self.ayo, &config.ayo, Some(initial.ayo)),
                            }
                        }
                        None => {
                            __MyFormDataSignal {
                                ayo: <u8 as ::leptos_form::FormField<
                                    <u8 as ::leptos_form::DefaultHtmlElement>::El,
                                >>::into_signal(self.ayo, &config.ayo, None),
                            }
                        }
                     }
                }
                fn try_from_signal(signal: Self::Signal, config: &Self::Config) -> Result<Self, #leptos_form_krate::FormError> {
                    Ok(MyFormData {
                        ayo: <u8 as #leptos_form_krate::FormField<<u8 as #leptos_form_krate::DefaultHtmlElement>::El>>::try_from_signal(signal.ayo, &config.ayo)?,
                    })
                }
                fn recurse(signal: &Self::Signal) {
                    <u8 as ::leptos_form::FormField<
                        <u8 as ::leptos_form::DefaultHtmlElement>::El,
                    >>::recurse(&signal.ayo);
                }
                fn reset_initial_value(signal: &Self::Signal) {
                    <u8 as #leptos_form_krate::FormField<<u8 as #leptos_form_krate::DefaultHtmlElement>::El>>::reset_initial_value(&signal.ayo);
                }
            }

            impl #leptos_form_krate::FormComponent<#leptos_krate::View> for MyFormData {
                #[allow(unused_imports)]
                fn render(props: #leptos_form_krate::RenderProps<Self::Signal, Self::Config>) -> impl #leptos_krate::IntoView {
                    use #leptos_form_krate::FormField;
                    use #leptos_krate::{IntoAttribute, IntoView};

                    let _ayo_id = #leptos_form_krate::format_form_id(props.id.as_ref(), #leptos_krate::Oco::Borrowed("ayo"));
                    let _ayo_name = #leptos_form_krate::format_form_name(props.name.as_ref(), "ayo");
                    let _ayo_props = #leptos_form_krate::RenderProps::builder()
                        .id(_ayo_id.clone())
                        .name(_ayo_name.clone())
                        .field_changed_class(props.field_changed_class.clone())
                        .signal(props.signal.ayo.clone())
                        .config(<u8 as #leptos_form_krate::FormField<<u8 as #leptos_form_krate::DefaultHtmlElement>::El>>::Config::default())
                        .build();

                    let _ayo_error = move || <u8 as #leptos_form_krate::FormField<<u8 as #leptos_form_krate::DefaultHtmlElement>::El>>::with_error(
                        &_ayo_props.signal,
                        |error| match error {
                            Some(form_error) => {
                                let error = format!("{form_error}");
                                #leptos_krate::IntoView::into_view(#leptos_krate::view! { <span style="color: red;">{error}</span> })
                            },
                            None => #leptos_krate::View::default(),
                        }
                    );
                    let ty = <::std::marker::PhantomData<(u8, <u8 as #leptos_form_krate::DefaultHtmlElement>::El)> as Default>::default();
                    let _ayo_view = #leptos_krate::view! { <FormField props=_ayo_props ty=ty /> };

                    #leptos_krate::view! {
                        <label for={_ayo_id}>
                            <div>"ayo"</div>
                            {_ayo_view}
                            {_ayo_error}
                        </label>
                    }
                }
            }

            pub use leptos_form_component_my_form_data::*;

            mod leptos_form_component_my_form_data {
                use super::*;
                use #leptos_krate::IntoView;
                use ::leptos_form::internal::wasm_bindgen::{
                    closure::Closure, JsCast, UnwrapThrowExt,
                };

                #[allow(unused_imports)]
                #[#leptos_krate::component]
                pub fn MyFormData(
                    mut initial: MyFormData,
                    #[prop(optional, into)]
                    top: Option<::leptos_form::components::LeptosFormChildren>,
                    #[prop(optional, into)]
                    bottom: Option<::leptos_form::components::LeptosFormChildren>,
                ) -> impl IntoView {
                    use #leptos_form_krate::{FormField, components::FormSubmissionHandler};
                    use #leptos_krate::{IntoAttribute, IntoView, SignalGet, SignalUpdate, SignalWith};
                    use ::leptos_form::internal::wasm_bindgen::UnwrapThrowExt;
                    use #leptos_router_krate::Form;
                    use ::std::rc::Rc;

                    let config = __MyFormDataConfig {
                        ayo: <u8 as #leptos_form_krate::FormField<<u8 as #leptos_form_krate::DefaultHtmlElement>::El>>::Config::default(),
                    };

                    let signal = #leptos_krate::create_rw_signal(#leptos_form_krate::RenderProps::builder()
                        .id(None)
                        .name("")
                        .signal(initial.clone().into_signal(&config, Some(initial.clone())))
                        .config(config.clone())
                        .build()
                    );
                    let _had_reset_called = ::leptos_form::internal::leptos::create_rw_signal(false);
                    let parse_error_handler = |err: #leptos_form_krate::FormError| #leptos_krate::logging::debug_warn!("{err}");
                    let ty = <::std::marker::PhantomData<(MyFormData, #leptos_krate::View)> as Default>::default();
                    #leptos_krate::view! {
                        <Form action="/api/my-form-data">
                            {top.map(|x| (x.0)())}
                            {move || #leptos_krate::view! { <FormField props=signal.get() ty=ty /> }}
                            {bottom.map(|x| (x.0)())}
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

    #[test]
    fn island_is_produced_correctly() -> Result<(), Error> {
        let input = quote!(
            #[derive(Form)]
            #[form(internal)]
            #[form(island(
                action = my_server_fn(my_form_data),
                map_submit = my_map_submit,
                reset_on_success,
            ))]
            #[form(label(wrap(
                id = "default-label-id",
                class = "default-label-class",
                style = "default-label-style",
                rename_all = "PascalCase"
            )))]
            pub struct MyFormData {
                #[form(label(adjacent(
                    container(
                        tag = "div",
                        id = "ayo-label-container-id",
                        class = "ayo-label-container-class",
                        style = "ayo-label-container-style"
                    ),
                    id = "ayo-label-id",
                    class = "ayo-label-class",
                    value = "AYO",
                )))]
                pub ayo: u8,
            }
        );

        // use internal for this test
        let leptos_form_krate = quote!(crate);
        let leptos_krate = quote!(#leptos_form_krate::internal::leptos);
        let leptos_router_krate = quote!(#leptos_form_krate::internal::leptos_router);
        let wasm_bindgen_krate = quote!(#leptos_form_krate::internal::wasm_bindgen);

        let expected = quote!(
            #[derive(Clone, Copy, Debug)]
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

                fn default_signal(config: &Self::Config, initial: Option<Self>) -> Self::Signal {
                    match initial {
                        Some(initial) => {
                            __MyFormDataSignal {
                                ayo: <u8 as crate::FormField<
                                    <u8 as crate::DefaultHtmlElement>::El,
                                >>::default_signal(&config.ayo, Some(initial.ayo)),
                            }
                        }
                        None => {
                            __MyFormDataSignal {
                                ayo: <u8 as crate::FormField<
                                    <u8 as crate::DefaultHtmlElement>::El,
                                >>::default_signal(&config.ayo, None),
                            }
                        }
                    }
                }
                fn is_initial_value(signal: &Self::Signal) -> bool {
                    true && <u8 as #leptos_form_krate::FormField<<u8 as #leptos_form_krate::DefaultHtmlElement>::El>>::is_initial_value(&signal.ayo)
                }
                fn into_signal(self, config: &Self::Config, initial: Option<Self>) -> Self::Signal {
                    match initial {
                        Some(initial) => {
                            __MyFormDataSignal {
                                ayo: <u8 as crate::FormField<
                                    <u8 as crate::DefaultHtmlElement>::El,
                                >>::into_signal(self.ayo, &config.ayo, Some(initial.ayo)),
                            }
                        }
                        None => {
                            __MyFormDataSignal {
                                ayo: <u8 as crate::FormField<
                                    <u8 as crate::DefaultHtmlElement>::El,
                                >>::into_signal(self.ayo, &config.ayo, None),
                            }
                        }
                    }
                }
                fn try_from_signal(signal: Self::Signal, config: &Self::Config) -> Result<Self, #leptos_form_krate::FormError> {
                    Ok(MyFormData {
                        ayo: <u8 as #leptos_form_krate::FormField<<u8 as #leptos_form_krate::DefaultHtmlElement>::El>>::try_from_signal(signal.ayo, &config.ayo)?,
                    })
                }
                fn recurse(signal: &Self::Signal) {
                    <u8 as crate::FormField<
                        <u8 as crate::DefaultHtmlElement>::El,
                    >>::recurse(&signal.ayo);
                }
                fn reset_initial_value(signal: &Self::Signal) {
                    <u8 as #leptos_form_krate::FormField<<u8 as #leptos_form_krate::DefaultHtmlElement>::El>>::reset_initial_value(&signal.ayo);
                }
            }

            impl #leptos_form_krate::FormComponent<#leptos_krate::View> for MyFormData {
                #[allow(unused_imports)]
                fn render(props: #leptos_form_krate::RenderProps<Self::Signal, Self::Config>) -> impl #leptos_krate::IntoView {
                    use #leptos_form_krate::FormField;
                    use #leptos_krate::{IntoAttribute, IntoView};

                    let _ayo_id = #leptos_form_krate::format_form_id(props.id.as_ref(), #leptos_krate::Oco::Borrowed("ayo"));
                    let _ayo_name = #leptos_form_krate::format_form_name(props.name.as_ref(), "ayo");
                    let _ayo_props = #leptos_form_krate::RenderProps::builder()
                        .id(_ayo_id.clone())
                        .name(_ayo_name.clone())
                        .field_changed_class(props.field_changed_class.clone())
                        .signal(props.signal.ayo.clone())
                        .config(<u8 as #leptos_form_krate::FormField<<u8 as #leptos_form_krate::DefaultHtmlElement>::El>>::Config::default())
                        .build();

                    let _ayo_error = move || <u8 as #leptos_form_krate::FormField<<u8 as #leptos_form_krate::DefaultHtmlElement>::El>>::with_error(
                        &_ayo_props.signal,
                        |error| match error {
                            Some(form_error) => {
                                let error = format!("{form_error}");
                                #leptos_krate::IntoView::into_view(#leptos_krate::view! { <span style="color: red;">{error}</span> })
                            },
                            None => #leptos_krate::View::default(),
                        }
                    );
                    let ty = <::std::marker::PhantomData<(u8, <u8 as #leptos_form_krate::DefaultHtmlElement>::El)> as Default>::default();
                    let _ayo_view = #leptos_krate::view! { <FormField props=_ayo_props ty=ty /> };

                    #leptos_krate::view! {
                        <div id="ayo-label-container-id" class="ayo-label-container-class" style="ayo-label-container-style">
                            <label for={_ayo_id} id="ayo-label-id" class="ayo-label-class" style="default-label-style">
                                "AYO"
                            </label>
                            {_ayo_view}
                            {_ayo_error}
                        </div>
                    }
                }
            }

            pub use leptos_form_component_my_form_data::*;

            mod leptos_form_component_my_form_data {
                use super::*;
                use #leptos_krate::IntoView;
                use crate::internal::wasm_bindgen::{closure::Closure, JsCast, UnwrapThrowExt};

                #[allow(unused_imports)]
                #[#leptos_krate::island]
                pub fn MyFormData(
                    mut initial: MyFormData,
                    #[prop(optional, into)]
                    top: Option<crate::components::LeptosFormChildren>,
                    #[prop(optional, into)]
                    bottom: Option<crate::components::LeptosFormChildren>,
                ) -> impl IntoView {
                    use #leptos_form_krate::{FormField, components::FormSubmissionHandler};
                    use #leptos_krate::{IntoAttribute, IntoView, SignalGet, SignalUpdate, SignalWith};
                    use #wasm_bindgen_krate::UnwrapThrowExt;
                    use #leptos_router_krate::Form;
                    use ::std::rc::Rc;

                    fn server_fn_inference<T: Clone, U>(f: impl Fn(T) -> U) -> impl Fn(&T) -> U {
                        move |data: &T| f(data.clone())
                    }
                    let action = #leptos_krate::create_action(server_fn_inference(my_server_fn));

                    let config = __MyFormDataConfig {
                        ayo: <u8 as #leptos_form_krate::FormField<<u8 as #leptos_form_krate::DefaultHtmlElement>::El>>::Config::default(),
                    };

                    let signal = #leptos_krate::create_rw_signal(#leptos_form_krate::RenderProps::builder()
                        .id(None)
                        .name(#leptos_krate::Oco::Borrowed("my_form_data"))
                        .signal(initial.clone().into_signal(&config, Some(initial.clone())))
                        .config(config.clone())
                        .build()
                    );
                    let _had_reset_called = crate::internal::leptos::create_rw_signal(false);
                    let parse_error_handler = |err: #leptos_form_krate::FormError| #leptos_krate::logging::debug_warn!("{err}");
                    #leptos_krate::create_effect({
                        let initial = initial.clone();
                        let action_value = action.value();
                        move |prev_value| {
                            let value = action_value.get();
                            if let None | Some(Err(_)) = value.as_ref() {
                                return None;
                            }
                            match prev_value {
                                None | Some(None) => {
                                    let config = __MyFormDataConfig {
                                        ayo: <u8 as crate::FormField<
                                            <u8 as crate::DefaultHtmlElement>::El,
                                        >>::Config::default(),
                                    };
                                    let new_props = crate::RenderProps::builder()
                                        .id(None)
                                        .name(crate::internal::leptos::Oco::Borrowed("my_form_data"))
                                        .signal(
                                            initial.clone().into_signal(&config, Some(initial.clone())),
                                        )
                                        .config(config.clone())
                                        .build();
                                    signal.update(move |props| *props = new_props);
                                    _had_reset_called.update(|x| *x = true);
                                    Some(value)
                                }
                                Some(Some(prev_value)) => None,
                            }
                        }
                    });
                    let ty = <::std::marker::PhantomData<(MyFormData, #leptos_krate::View)> as Default>::default();
                    #leptos_krate::view! {
                        <Form
                            action="/api/my_server_fn"
                            on:submit=move |ev| {
                                ev.prevent_default();
                                let data = match signal.with(|props| <MyFormData as #leptos_form_krate::FormField<#leptos_krate::View>>::try_from_signal(props.signal, &config)) {
                                    Ok(parsed) => parsed,
                                    Err(err) => {
                                        parse_error_handler(err);
                                        return;
                                    },
                                };
                                let data = my_map_submit(#leptos_form_krate::FormDiff { initial: initial.clone(), current: data });
                                action.dispatch(data);
                            }
                        >
                            {top.map(|x| (x.0)())}
                            {move || #leptos_krate::view! { <FormField props=signal.get() ty=ty /> }}
                            {bottom.map(|x| (x.0)())}
                            <FormSubmissionHandler
                                action=action
                                error_view_ty={<::std::marker::PhantomData<#leptos_krate::View> as Default>::default()}
                                success_view_ty={<::std::marker::PhantomData<#leptos_krate::View> as Default>::default()}
                            />
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

    #[test]
    fn custom_config_is_correctly_provided() -> Result<(), Error> {
        let leptos_form_krate = quote!(::leptos_form);
        let leptos_krate = quote!(#leptos_form_krate::internal::leptos);
        let leptos_router_krate = quote!(#leptos_form_krate::internal::leptos_router);
        let wasm_bindgen_krate = quote!(#leptos_form_krate::internal::wasm_bindgen);

        let input = quote!(
            #[derive(Form)]
            #[form(component(action = "/api/my-form-data"))]
            pub struct MyFormData {
                #[form(config = #leptos_form_krate::NaiveDateTimeConfig { format: "%FT%T" })]
                pub created_at: chrono::NaiveDateTime,
            }
        );

        let expected = quote!(
            #[derive(Clone, Copy, Debug)]
            pub struct __MyFormDataSignal {
                pub created_at: <chrono::NaiveDateTime as #leptos_form_krate::FormField<<chrono::NaiveDateTime as #leptos_form_krate::DefaultHtmlElement>::El>>::Signal,
            }

            #[derive(Clone, Debug, Default)]
            pub struct __MyFormDataConfig {
                pub created_at: <chrono::NaiveDateTime as #leptos_form_krate::FormField<<chrono::NaiveDateTime as #leptos_form_krate::DefaultHtmlElement>::El>>::Config,
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
                fn default_signal(config: &Self::Config, initial: Option<Self>) -> Self::Signal {
                    match initial {
                        Some(initial) => {
                            __MyFormDataSignal {
                                created_at: <chrono::NaiveDateTime as ::leptos_form::FormField<
                                    <chrono::NaiveDateTime as ::leptos_form::DefaultHtmlElement>::El,
                                >>::default_signal(&config.created_at, Some(initial.created_at)),
                            }
                        }
                        None => {
                            __MyFormDataSignal {
                                created_at: <chrono::NaiveDateTime as ::leptos_form::FormField<
                                    <chrono::NaiveDateTime as ::leptos_form::DefaultHtmlElement>::El,
                                >>::default_signal(&config.created_at, None),
                            }
                        }
                    }
                }
                fn is_initial_value(signal: &Self::Signal) -> bool {
                    true && <chrono::NaiveDateTime as #leptos_form_krate::FormField<<chrono::NaiveDateTime as #leptos_form_krate::DefaultHtmlElement>::El>>::is_initial_value(&signal.created_at)
                }
                fn into_signal(self, config: &Self::Config, initial: Option<Self>) -> Self::Signal {
                    match initial {
                        Some(initial) => {
                            __MyFormDataSignal {
                                created_at: <chrono::NaiveDateTime as ::leptos_form::FormField<
                                    <chrono::NaiveDateTime as ::leptos_form::DefaultHtmlElement>::El,
                                >>::into_signal(
                                    self.created_at,
                                    &config.created_at,
                                    Some(initial.created_at),
                                ),
                            }
                        }
                        None => {
                            __MyFormDataSignal {
                                created_at: <chrono::NaiveDateTime as ::leptos_form::FormField<
                                    <chrono::NaiveDateTime as ::leptos_form::DefaultHtmlElement>::El,
                                >>::into_signal(self.created_at, &config.created_at, None),
                            }
                        }
                    }
                }
                fn try_from_signal(signal: Self::Signal, config: &Self::Config) -> Result<Self, #leptos_form_krate::FormError> {
                    Ok(MyFormData {
                        created_at: <chrono::NaiveDateTime as #leptos_form_krate::FormField<<chrono::NaiveDateTime as #leptos_form_krate::DefaultHtmlElement>::El>>::try_from_signal(signal.created_at, &config.created_at)?,
                    })
                }
                fn recurse(signal: &Self::Signal) {
                    <chrono::NaiveDateTime as ::leptos_form::FormField<
                        <chrono::NaiveDateTime as ::leptos_form::DefaultHtmlElement>::El,
                    >>::recurse(&signal.created_at);
                }
                fn reset_initial_value(signal: &Self::Signal) {
                    <chrono::NaiveDateTime as #leptos_form_krate::FormField<<chrono::NaiveDateTime as #leptos_form_krate::DefaultHtmlElement>::El>>::reset_initial_value(&signal.created_at);
                }
            }

            impl #leptos_form_krate::FormComponent<#leptos_krate::View> for MyFormData {
                #[allow(unused_imports)]
                fn render(props: #leptos_form_krate::RenderProps<Self::Signal, Self::Config>) -> impl #leptos_krate::IntoView {
                    use #leptos_form_krate::FormField;
                    use #leptos_krate::{IntoAttribute, IntoView};

                    let _created_at_id = #leptos_form_krate::format_form_id(props.id.as_ref(), #leptos_krate::Oco::Borrowed("created-at"));
                    let _created_at_name = #leptos_form_krate::format_form_name(props.name.as_ref(), "created_at");
                    let _created_at_props = #leptos_form_krate::RenderProps::builder()
                        .id(_created_at_id.clone())
                        .name(_created_at_name.clone())
                        .field_changed_class(props.field_changed_class.clone())
                        .signal(props.signal.created_at.clone())
                        .config(#leptos_form_krate::NaiveDateTimeConfig { format: "%FT%T" })
                        .build();

                    let _created_at_error = move || <chrono::NaiveDateTime as #leptos_form_krate::FormField<<chrono::NaiveDateTime as #leptos_form_krate::DefaultHtmlElement>::El>>::with_error(
                        &_created_at_props.signal,
                        |error| match error {
                            Some(form_error) => {
                                let error = format!("{form_error}");
                                #leptos_krate::IntoView::into_view(#leptos_krate::view! { <span style="color: red;">{error}</span> })
                            },
                            None => #leptos_krate::View::default(),
                        },
                    );
                    let ty = <::std::marker::PhantomData<(chrono::NaiveDateTime, <chrono::NaiveDateTime as #leptos_form_krate::DefaultHtmlElement>::El)> as Default>::default();
                    let _created_at_view = #leptos_krate::view! { <FormField props=_created_at_props ty=ty /> };

                    #leptos_krate::view! {
                        <label for={_created_at_id}>
                            <div>"created_at"</div>
                            {_created_at_view}
                            {_created_at_error}
                        </label>
                    }
                }
            }

            pub use leptos_form_component_my_form_data::*;
            mod leptos_form_component_my_form_data {
                use super::*;
                use #leptos_krate::IntoView;
                use #wasm_bindgen_krate::{closure::Closure, JsCast, UnwrapThrowExt,};
                #[allow(unused_imports)]
                #[#leptos_krate::component]
                pub fn MyFormData(
                    mut initial: MyFormData,
                    #[prop(optional, into)]
                    top: Option<#leptos_form_krate::components::LeptosFormChildren>,
                    #[prop(optional, into)]
                    bottom: Option<#leptos_form_krate::components::LeptosFormChildren>,
                ) -> impl IntoView {
                    use #leptos_form_krate::{FormField, components::FormSubmissionHandler};
                    use #leptos_krate::{IntoAttribute, IntoView, SignalGet, SignalUpdate, SignalWith,};
                    use #wasm_bindgen_krate::UnwrapThrowExt;
                    use #leptos_router_krate::Form;
                    use ::std::rc::Rc;
                    let config = __MyFormDataConfig {
                        created_at: #leptos_form_krate::NaiveDateTimeConfig {
                            format: "%FT%T",
                        },
                    };
                    let signal = #leptos_krate::create_rw_signal(
                        #leptos_form_krate::RenderProps::builder()
                            .id(None)
                            .name("")
                            .signal(initial.clone().into_signal(&config, Some(initial.clone())))
                            .config(config.clone())
                            .build(),
                    );
                    let _had_reset_called = #leptos_krate::create_rw_signal(false);
                    let parse_error_handler = |err: #leptos_form_krate::FormError| {
                        #leptos_krate::logging::debug_warn!("{err}")
                    };
                    let ty = <::std::marker::PhantomData<
                        (MyFormData, #leptos_krate::View),
                    > as Default>::default();
                    #leptos_krate::view! {
                        < Form action = "/api/my-form-data" > { top.map(| x | (x.0)()) } { move ||
                        #leptos_krate::view! { < FormField props =signal.get() ty
                        = ty / > } } { bottom.map(| x | (x.0) ()) } < / Form >
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
}
