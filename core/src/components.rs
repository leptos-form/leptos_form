//! Common form components

use leptos::*;
use std::rc::Rc;

pub trait OnError<I: 'static, T: Clone + 'static, IV: IntoView + 'static>:
    Fn(ServerFnError, Action<I, Result<T, ServerFnError>>) -> IV + 'static
{
}

pub trait OnSuccess<I: 'static, T: Clone + 'static, IV: IntoView + 'static>:
    Fn(T, Action<I, Result<T, ServerFnError>>) -> IV + 'static
{
}

impl<I: 'static, T: Clone + 'static, IV: IntoView + 'static, F> OnError<I, T, IV> for F where
    F: Fn(ServerFnError, Action<I, Result<T, ServerFnError>>) -> IV + 'static
{
}
impl<I: 'static, T: Clone + 'static, IV: IntoView + 'static, F> OnSuccess<I, T, IV> for F where
    F: Fn(T, Action<I, Result<T, ServerFnError>>) -> IV + 'static
{
}

#[component]
pub fn FormSubmissionHandler<IV1: IntoView + 'static, IV2: IntoView + 'static, I: 'static, T: Clone + 'static>(
    action: Action<I, Result<T, ServerFnError>>,
    #[prop(optional)] on_error: Option<Rc<dyn OnError<I, T, IV2>>>,
    #[prop(optional)] on_success: Option<Rc<dyn OnSuccess<I, T, IV1>>>,
    #[allow(unused_variables)]
    #[prop(optional)]
    error_view_ty: Option<std::marker::PhantomData<IV2>>,
    #[allow(unused_variables)]
    #[prop(optional)]
    success_view_ty: Option<std::marker::PhantomData<IV1>>,
) -> impl IntoView {
    view! {{move || match action.pending().get() {
        true => view! { <div>"Loading..."</div> }.into_view(),
        false => match action.value().get() {
            Some(Ok(ok)) => match on_success.clone() {
                Some(on_success) => on_success(ok, action).into_view(),
                None => View::default(),
            },
            Some(Err(err)) => match on_error.clone() {
                Some(on_error) => on_error(err, action).into_view(),
                None => view! {
                    <div>
                    {move || match err.clone() {
                        ServerFnError::Request(err) => err,
                        ServerFnError::ServerError(err) => err,
                        _ => "Internal Error".to_string(),
                    }}
                    </div>
                }.into_view(),
            },
            None => View::default(),
        }
    }}}
}

/// Aderived signal returning a style string which should be placed on the top level component's `style:opacity` prop
pub type StyleSignal = Rc<dyn Fn() -> Option<&'static str>>;

#[component]
pub fn MaterialIcon(
    d: &'static str,
    #[prop(optional_no_strip, into)] class: Option<Oco<'static, str>>,
    #[prop(optional_no_strip, into)] cursor: Option<StyleSignal>,
    #[prop(optional_no_strip, into)] height: Option<usize>,
    #[prop(optional_no_strip, into)] opacity: Option<StyleSignal>,
    #[prop(optional_no_strip, into)] style: Option<Oco<'static, str>>,
    #[prop(optional_no_strip, into)] width: Option<usize>,
) -> impl IntoView {
    let transform = svg_transform(24, 24, height, width);
    let style = match (style, transform) {
        (Some(style), Some(transform)) => Some(format!("{transform} {style}")),
        (Some(style), None) => Some(style.to_string()),
        (None, Some(transform)) => Some(transform),
        (None, None) => None,
    };
    let cursor = move || cursor.clone().and_then(|x| x());
    let opacity = move || opacity.clone().and_then(|x| x());
    view! {
        <svg
            class={class}
            xmlns="http://www.w3.org/2000/svg"
            height="24"
            width="24"
            viewBox="0 -960 960 960"
            fill="currentColor"
            style:cursor=cursor
            style:opacity=opacity
            style={style}
        >
            <path d={d} />
        </svg>
    }
}

fn svg_transform(
    default_height: usize,
    default_width: usize,
    height: Option<usize>,
    width: Option<usize>,
) -> Option<String> {
    let height = height.unwrap_or(default_height);
    let width = width.unwrap_or(default_width);

    let xscale = (width != default_width).then_some(width as f32 / default_width as f32);
    let yscale = (height != default_height).then_some(height as f32 / default_height as f32);
    if xscale.is_none() && yscale.is_none() {
        return None;
    }

    let xtranslate = xscale.map(|xscale| (xscale - 1.) * default_width as f32 / 2.);
    let ytranslate = yscale.map(|yscale| (yscale - 1.) * default_height as f32 / 2.);

    let xscale = xscale.map(|val| val.to_string()).unwrap_or_default();
    let yscale = yscale.map(|val| val.to_string()).unwrap_or_default();
    let xtranslate = xtranslate.map(|val| val.to_string()).unwrap_or_default();
    let ytranslate = ytranslate.map(|val| val.to_string()).unwrap_or_default();

    Some(format!(
        "transform: translate({xtranslate} {ytranslate}) scale({xscale} {yscale});"
    ))
}

#[component]
pub fn MaterialClose(
    #[prop(optional_no_strip, into)] class: Option<Oco<'static, str>>,
    #[prop(optional_no_strip, into)] cursor: Option<StyleSignal>,
    #[prop(optional_no_strip, into)] height: Option<usize>,
    #[prop(optional_no_strip, into)] opacity: Option<StyleSignal>,
    #[prop(optional_no_strip, into)] style: Option<Oco<'static, str>>,
    #[prop(optional_no_strip, into)] width: Option<usize>,
) -> impl IntoView {
    view! {
        <MaterialIcon
            class=class
            cursor=cursor
            d="m256-200-56-56 224-224-224-224 56-56 224 224 224-224 56 56-224 224 224 224-56 56-224-224-224 224Z"
            height=height
            opacity=opacity
            style=style
            width=width
        />
    }
}
