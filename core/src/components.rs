use leptos::*;

#[component]
pub fn MaterialIcon(
    d: &'static str,
    #[prop(optional_no_strip, into)] class: Option<Oco<'static, str>>,
    #[prop(optional_no_strip, into)] style: Option<Oco<'static, str>>,
    #[prop(optional_no_strip, into)] height: Option<usize>,
    #[prop(optional_no_strip, into)] width: Option<usize>,
) -> impl IntoView {
    let transform = svg_transform(24, 24, height, width);
    let style = match (style, transform) {
        (Some(style), Some(transform)) => Some(format!("{transform} {style}")),
        (Some(style), None) => Some(style.to_string()),
        (None, Some(transform)) => Some(transform),
        (None, None) => None,
    };
    view! {
        <svg
            class={class}
            xmlns="http://www.w3.org/2000/svg"
            height="24"
            width="24"
            viewBox="0 -960 960 960"
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
    #[prop(optional_no_strip, into)] style: Option<Oco<'static, str>>,
    #[prop(optional_no_strip, into)] height: Option<usize>,
    #[prop(optional_no_strip, into)] width: Option<usize>,
) -> impl IntoView {
    view! {
        <MaterialIcon
            d="m256-200-56-56 224-224-224-224 56-56 224 224 224-224 56 56-224 224 224 224-56 56-224-224-224 224Z"
            class={class}
            height={height}
            style={style}
            width={width}
        />
    }
}