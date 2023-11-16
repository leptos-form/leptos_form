#![allow(unused)]

use crate::*;
use ::leptos::html::*;
use ::leptos::*;
use ::wasm_bindgen::JsValue;

#[cfg(feature = "uuid")]
mod uuid {
    use super::*;

    impl DefaultHtmlElement for ::uuid::Uuid {
        type El = HtmlElement<Input>;
    }

    impl FormField<HtmlElement<Input>> for ::uuid::Uuid {
        type Config = ();
        type Signal = FormFieldSignal<String>;

        fn default_signal(config: &Self::Config, initial: Option<Self>) -> Self::Signal {
            FormFieldSignal::new_with_default_value(initial.map(|x| x.to_string()))
        }
        fn is_initial_value(signal: &Self::Signal) -> bool {
            signal.value.with(|value| {
                signal.initial.with(|initial| match initial {
                    Some(initial) => initial == value,
                    None => value.is_empty(),
                })
            })
        }
        fn into_signal(self, _: &Self::Config, initial: Option<Self>) -> Self::Signal {
            FormFieldSignal::new(self.to_string(), initial.map(|x| x.to_string()))
        }
        fn try_from_signal(signal: Self::Signal, _: &Self::Config) -> Result<Self, FormError> {
            use std::str::FromStr;
            signal
                .value
                .with(|value| ::uuid::Uuid::from_str(value))
                .map_err(FormError::parse)
        }
        fn recurse(signal: &Self::Signal) {
            signal.value.with(|_| {})
        }
        fn reset_initial_value(signal: &Self::Signal) {
            signal
                .value
                .with(|value| signal.initial.update(|initial| *initial = Some(value.clone())));
        }
        fn with_error<O>(signal: &Self::Signal, f: impl FnOnce(Option<&FormError>) -> O) -> O {
            signal.error.with(|error| f(error.as_ref()))
        }
    }

    impl FormComponent<HtmlElement<Input>> for ::uuid::Uuid {
        fn render(props: RenderProps<Self::Signal, Self::Config>) -> impl IntoView {
            let class = props.class_signal();
            view! {
                <input
                    type="text"
                    class={class}
                    id={props.id.or_else(|| props.name.clone())}
                    name={props.name}
                    on:input=move |ev| props.signal.value.update(|value| *value = event_target_value(&ev))
                    on:change=move |_| {
                        if let Err(form_error) = <Self as FormField<HtmlElement<Input>>>::try_from_signal(props.signal, &props.config) {
                            props.signal.error.update(|error| *error = Some(form_error));
                        } else if props.signal.error.with_untracked(|error| error.is_some()) {
                            props.signal.error.update(|error| *error = None);
                        }
                    }
                    prop:class={move || class.with(|x| x.as_ref().map(|x| JsValue::from_str(x)))}
                    prop:value={props.signal.value}
                    style={props.style}
                    value=props.signal.value
                />
            }
        }
    }
}

#[cfg(feature = "chrono")]
/// Configuration utilities for using [`chrono`](::chrono) types in form types.
pub mod chrono {
    use super::*;
    use ::chrono::*;

    #[derive(Clone, Debug)]
    pub struct NaiveDateConfig {
        pub format: &'static str,
    }
    #[derive(Clone, Debug)]
    pub struct NaiveDateTimeConfig {
        pub format: &'static str,
    }
    #[derive(Clone, Debug)]
    pub struct FixedOffsetDateTimeConfig {
        pub format: &'static str,
    }
    #[derive(Clone, Debug)]
    pub struct UtcDateTimeConfig {
        pub format: &'static str,
    }
    #[derive(Clone, Debug)]
    pub struct LocalDateTimeConfig {
        pub format: &'static str,
    }

    impl Default for NaiveDateConfig {
        fn default() -> Self {
            Self { format: "%x" }
        }
    }
    impl Default for NaiveDateTimeConfig {
        fn default() -> Self {
            Self { format: "%c" }
        }
    }
    impl Default for FixedOffsetDateTimeConfig {
        fn default() -> Self {
            Self { format: "%+" }
        }
    }
    impl Default for UtcDateTimeConfig {
        fn default() -> Self {
            Self { format: "%c" }
        }
    }
    impl Default for LocalDateTimeConfig {
        fn default() -> Self {
            Self { format: "%c" }
        }
    }

    macro_rules! chrono_impl {
        ($($ty:ty, $config:ty { $($from:tt)* }),*$(,)?) => { $(
            chrono_impl! { @ $ty, $config { $($from)* } }
        )* };

        (@ $ty:ty, $config:ty { $($from:tt)* }) => { paste! {
            impl DefaultHtmlElement for $ty {
                type El = HtmlElement<Input>;
            }

            impl FormField<HtmlElement<Input>> for $ty {
                type Config = $config;
                type Signal = FormFieldSignal<String>;

                fn default_signal(config: &Self::Config, initial: Option<Self>) -> Self::Signal {
                    FormFieldSignal::new_with_default_value(initial.map(|x| x.format(config.format).to_string()))
                }
                fn is_initial_value(signal: &Self::Signal) -> bool {
                    signal.value.with(|value| signal.initial.with(|initial| match initial {
                        Some(initial) => initial == value,
                        None => value.is_empty(),
                    }))
                }
                fn into_signal(self, config: &Self::Config, initial: Option<Self>) -> Self::Signal {
                    FormFieldSignal::new(self.format(config.format).to_string(), initial.map(|initial| initial.format(config.format).to_string()))
                }
                fn recurse(signal: &Self::Signal) {
                    signal.with(|_| {})
                }
                fn reset_initial_value(signal: &Self::Signal) {
                    signal.value.with(|value| signal.initial.update(|initial| *initial = Some(value.clone())));
                }
                fn with_error<O>(signal: &Self::Signal, f: impl FnOnce(Option<&FormError>) -> O) -> O {
                    signal.error.with(|error| f(error.as_ref()))
                }

                $($from)*
            }

            impl FormComponent<HtmlElement<Input>> for $ty {
                fn render(props: RenderProps<Self::Signal, Self::Config>) -> impl IntoView {
                    let class = props.class_signal();
                    view! {
                        <input
                            type="text"
                            class={class}
                            id={props.id.or_else(|| props.name.clone())}
                            name={props.name}
                            on:input=move |ev| props.signal.value.update(|value| *value = event_target_value(&ev))
                            on:change=move |_| {
                                if let Err(form_error) = <Self as FormField<HtmlElement<Input>>>::try_from_signal(props.signal, &props.config) {
                                    props.signal.error.update(|error| *error = Some(form_error));
                                } else if props.signal.error.with_untracked(|error| error.is_some()) {
                                    props.signal.error.update(|error| *error = None);
                                }
                            }
                            prop:class={move || class.with(|x| x.as_ref().map(|x| JsValue::from_str(&*x)))}
                            prop:value={props.signal.value}
                            style={props.style}
                            value=props.signal.value
                        />
                    }
                }
            }
        } };
    }

    chrono_impl!(
        NaiveDate, NaiveDateConfig {
            fn try_from_signal(signal: Self::Signal, config: &Self::Config) -> Result<Self, FormError> {
                signal.value.with(|value| Self::parse_from_str(value, config.format)).map_err(FormError::parse)
            }
        },
        NaiveDateTime, NaiveDateTimeConfig {
            fn try_from_signal(signal: Self::Signal, config: &Self::Config) -> Result<Self, FormError> {
                signal.value.with(|value| Self::parse_from_str(value, config.format)).map_err(FormError::parse)
            }
        },
        DateTime<FixedOffset>, FixedOffsetDateTimeConfig {
            fn try_from_signal(signal: Self::Signal, config: &Self::Config) -> Result<Self, FormError> {
                signal.value.with(|value| DateTime::parse_from_str(value, config.format)).map_err(FormError::parse)
            }
        },
        DateTime<Utc>, UtcDateTimeConfig {
            fn try_from_signal(signal: Self::Signal, config: &Self::Config) -> Result<Self, FormError> {
                signal.value.with(|value| DateTime::parse_from_str(value, config.format).map(|value| value.with_timezone(&Utc))).map_err(FormError::parse)
            }
        },
        DateTime<Local>, LocalDateTimeConfig {
            fn try_from_signal(signal: Self::Signal, config: &Self::Config) -> Result<Self, FormError> {
                signal.value.with(|value| DateTime::parse_from_str(value, config.format).map(|value| value.with_timezone(&Local))).map_err(FormError::parse)
            }
        },
    );
}
