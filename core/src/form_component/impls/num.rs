use crate::*;
use ::leptos::html::*;
use ::leptos::*;

macro_rules! int_impl {
    ($($ty:ty),*$(,)?) => { $(
        impl DefaultHtmlElement for $ty {
            type El = HtmlElement<Input>;
        }

        impl FormSignalType<HtmlElement<Input>> for $ty {
            type Config = ();
            type SignalType = RwSignal<String>;

            fn default_signal() -> Self::SignalType {
                RwSignal::new(Default::default())
            }
            fn is_default_value(signal: &Self::SignalType) -> bool {
                signal.with(|value| value.is_empty())
            }
            fn into_signal_type(self, _: &Self::Config) -> Self::SignalType {
                RwSignal::new(self.to_string())
            }
            fn try_from_signal_type(signal_type: Self::SignalType, _: &Self::Config) -> Result<Self, FormError> {
                signal_type.get().parse().map_err(FormError::parse)
            }
        }

        impl FormComponent<HtmlElement<Input>> for $ty {
            fn render(props: RenderProps<Self::SignalType, Self::Config>) -> impl IntoView {
                props.signal.with(|value| {
                    view! {
                        <input
                            id={props.id.unwrap_or_else(|| props.name.clone())}
                            name={props.name}
                            class={props.class}
                            type="number"
                            min={$ty::MIN}
                            max={$ty::MAX}
                            value={value}
                            on:change=setter(props.signal, |x| x.as_string())
                        />
                    }
                })
            }
        }
    )* };
}

int_impl!(u8, u16, u32, u64, u128, usize, i8, i16, i32, i64, i128, isize);
