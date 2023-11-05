use crate::*;
use ::leptos::html::*;
use ::leptos::*;
use ::wasm_bindgen::JsValue;

macro_rules! num_impl {
    ($($ty:ty $({ $(type: $type:literal)?$(,)? $(min: $min:expr, max: $max:expr)?$(,)? })? ),*$(,)?) => { $(
        impl DefaultHtmlElement for $ty {
            type El = HtmlElement<Input>;
        }

        impl FormField<HtmlElement<Input>> for $ty {
            type Config = ();
            type Signal = FormFieldSignal<String>;

            fn default_signal() -> Self::Signal {
                Default::default()
            }
            fn is_default_value(signal: &Self::Signal) -> bool {
                signal.with(|x| x.value.is_empty())
            }
            fn into_signal(self, _: &Self::Config) -> Self::Signal {
                Self::Signal::from(self.to_string())
            }
            fn try_from_signal(signal: Self::Signal, _: &Self::Config) -> Result<Self, FormError> {
                signal.with(|x| x.value.parse()).map_err(FormError::parse)
            }
            fn reset_initial_value(signal: &Self::Signal) {
                signal.update(|sig| sig.initial = Some(sig.value.clone()));
            }
            fn with_error<O>(signal: &Self::Signal, f: impl FnOnce(Option<&FormError>) -> O) -> O {
                signal.with(|x| f(x.error.as_ref()))
            }
        }

        impl FormComponent<HtmlElement<Input>> for $ty {
            fn render(props: RenderProps<Self::Signal, Self::Config>) -> impl IntoView {
                let class = props.class_signal();
                view! {
                    <input
                        type=num_impl!(@type $($($type)?)?)
                        class={class}
                        id={props.id.or_else(|| props.name.clone())}
                        max=num_impl!(@max $ty $($(, $max)?)?)
                        min=num_impl!(@min $ty $($(, $min)?)?)
                        name={props.name}
                        on:keydown=num_impl!(@prevent_invalid_keystrokes value $($($type)?)?)
                        on:input=move |ev| props.signal.0.update(|x| x.value = event_target_value(&ev))
                        on:change=move |_| {
                            if let Err(form_error) = <Self as FormField<HtmlElement<Input>>>::try_from_signal(props.signal, &props.config) {
                                props.signal.update(|x| x.error = Some(form_error));
                            } else if props.signal.with_untracked(|x| x.error.is_some()) {
                                props.signal.update(|x| x.error = None);
                            }
                        }
                        prop:class={move || class.with(|x| x.as_ref().map(|x| JsValue::from_str(&*x)))}
                        prop:value={props.signal.0}
                        style={props.style}
                        value=move || props.signal.get().value
                    />
                }
            }
        }
    )* };
    (@type $type:literal) => {$type};
    (@type) => {"number"};

    (@min $ty:ty, $min:expr) => {$min};
    (@min $ty:ty) => {<$ty>::MIN};

    (@max $ty:ty, $max:expr) => {$max};
    (@max $ty:ty) => {<$ty>::MAX};

    (@prevent_invalid_keystrokes $value:ident $type:literal) => {|ev| {
        let key = ev.key();
        if key.len() > 1 {
            return;
        }
        if let Some(c) = key.chars().next() {
            if !matches!(c, '0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'.'|'-'|'+') {
                ev.prevent_default();
            }
        }
    }};
    (@prevent_invalid_keystrokes $value:ident) => {|_| {}};
}

num_impl!(
    u8,
    u16,
    u32,
    u64,
    u128,
    usize,
    i8,
    i16,
    i32,
    i64,
    i128,
    isize,
    f32 { type: "text" },
    f64 { type: "text" },
);

#[cfg(feature = "num-bigint")]
num_impl!(
    num_bigint::BigInt {
        min: None::<&'static str>,
        max: None::<&'static str>
    },
    num_bigint::BigUint {
        min: "0",
        max: None::<&'static str>
    },
);

#[cfg(feature = "bigdecimal")]
num_impl!(
    bigdecimal::BigDecimal { type: "text", min: None::<&'static str>, max: None::<&'static str> },
);
