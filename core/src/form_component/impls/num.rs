use crate::*;
use ::leptos::*;
use ::leptos::html::*;

macro_rules! int_impl {
    ($($ty:ty),*$(,)?) => { $(
        impl DefaultHtmlElement for $ty {
            type El = HtmlElement<Input>;
        }

        impl FormSignalType<HtmlElement<Input>> for $ty {
            type Config = ();
            type SignalType = String;
            fn into_signal_type(self, _: Self::Config) -> Self::SignalType {
                self.to_string()
            }
            fn try_from_signal_type(signal_type: Self::SignalType, _: Self::Config) -> Result<Self, FormError> {
                signal_type.parse().map_err(FormError::parse)
            }
        }

        impl<T: 'static> FormComponent<T, HtmlElement<Input>> for $ty {
            fn render(props: RenderProps<T, impl RefAccessor<T, Self::SignalType>, impl MutAccessor<T, Self::SignalType>, Self::Config>) -> impl IntoView {
                props.signal.with(|t| {
                    let value = (props.ref_ax)(t);
                    view! {
                        <input
                            id={props.id.unwrap_or_else(|| props.name.clone())}
                            name={props.name}
                            class={props.class}
                            type="number"
                            min={$ty::MIN}
                            max={$ty::MAX}
                            value={value}
                            on:change=setter(props.signal, props.mut_ax, |x| x.as_string())
                        />
                    }
                })
            }
        }
    )* };
}

int_impl!(u8, u16, u32, u64, u128, usize, i8, i16, i32, i64, i128, isize);

