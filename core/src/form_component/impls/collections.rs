use crate::components::*;
use crate::*;
use ::core::ops::*;
use ::indexmap::IndexMap;
use ::std::rc::Rc;

/// Configuration for a Vec of FormFields
#[derive(Clone, Default, Derivative, TypedBuilder)]
#[builder(field_defaults(default, setter(into)))]
#[derivative(Debug)]
pub struct VecConfig<Config: Default> {
    /// item level configuration, specific to the inner type of Vec
    /// for which this config is being applied
    pub item: Config,
    /// custom class to be passed into each item's FormField props
    pub item_class: Option<&'static str>,
    /// custom style to be passed into each item's FormField props
    pub item_style: Option<&'static str>,
    /// label configuration for each item
    #[builder(setter(strip_option))]
    pub item_label: Option<VecItemLabel>,
    /// Vec size control for this form
    pub size: VecConfigSize,
    /// configuration for an add button at the end of the listed items
    pub add: Adornment,
    /// configuration for the remove buttons adorning each field
    pub remove: Adornment,
}

/// Label configuration which will be set for each item
/// in a Vec of FormFields
#[derive(Clone, Copy, Debug, Default, TypedBuilder)]
#[builder(field_defaults(default, setter(into)))]
pub struct VecItemLabel {
    /// custom class to set for each label
    pub class: Option<&'static str>,
    /// notation variant for the item label (determines the label content)
    pub notation: VecItemLabelNotation,
    /// punctuation style for the item label
    pub punctuation: VecItemLabelPunctuation,
    /// custom style to set for each label
    pub style: Option<&'static str>,
}

/// When a label is configured to be set for a Vec of form fields,
/// the label used will be produced using one of the below notations.
#[derive(Clone, Copy, Debug, Default)]
pub enum VecItemLabelNotation {
    CapitalLetter,
    Letter,
    #[default]
    Number,
}

#[derive(Clone, Copy, Debug, Default)]
pub enum VecItemLabelPunctuation {
    #[default]
    None,
    Parenthesis,
    Period,
}

#[derive(Clone, Copy, Debug, Default)]
pub enum VecConfigSize {
    Bounded {
        min: Option<usize>,
        max: Option<usize>,
    },
    Const(usize),
    #[default]
    Unbounded,
}

#[derive(Clone, Default, Derivative)]
#[derivative(Debug)]
pub enum Adornment {
    None,
    #[default]
    Default,
    Component(#[derivative(Debug = "ignore")] AdornmentComponent),
    Spec(AdornmentSpec),
}

/// A Component type which accepts two arguments:
/// - an on:click callback which accepts a MouseEvent argument
/// - a derived signal returning a style string which should
///   be placed on the top level component's `style:opacity` prop
pub type AdornmentComponent = Rc<dyn Fn(Rc<dyn Fn(web_sys::MouseEvent)>, StyleSignal) -> View + 'static>;

#[derive(Clone, Debug, TypedBuilder)]
#[builder(field_defaults(default, setter(into)))]
pub struct AdornmentSpec {
    #[builder(setter(strip_option))]
    pub class: Option<Oco<'static, str>>,
    #[builder(default = 24)]
    pub height: usize,
    #[builder(setter(strip_option))]
    pub style: Option<Oco<'static, str>>,
    pub text: Option<Oco<'static, str>>,
    #[builder(default = 24)]
    pub width: usize,
}

impl Default for AdornmentSpec {
    fn default() -> Self {
        Self {
            class: None,
            height: 24,
            style: None,
            text: None,
            width: 24,
        }
    }
}

impl<T: DefaultHtmlElement> DefaultHtmlElement for Vec<T> {
    type El = Vec<T::El>;
}

#[derive(Clone, Copy, Debug)]
pub struct VecSignalItem<Signal> {
    id: usize,
    signal: Signal,
}

impl<T, El> FormField<Vec<El>> for Vec<T>
where
    T: FormField<El>,
    <T as FormField<El>>::Signal: Clone,
{
    type Config = VecConfig<<T as FormField<El>>::Config>;
    type Signal = FormFieldSignal<IndexMap<usize, VecSignalItem<<T as FormField<El>>::Signal>>>;

    fn default_signal() -> Self::Signal {
        Default::default()
    }
    fn is_default_value(signal: &Self::Signal) -> bool {
        signal.with(|sig| match sig.initial.as_ref() {
            Some(initial) => {
                let no_keys_changed =
                    sig.value.len() == initial.len() && sig.value.last().map(|x| x.0) == initial.last().map(|x| x.0);
                if !no_keys_changed {
                    return false;
                }
                sig.value.iter().all(|(_, x)| T::is_default_value(&x.signal))
            }
            None => sig.value.is_empty(),
        })
    }
    fn into_signal(self, config: &Self::Config) -> Self::Signal {
        Self::Signal::from(
            self.into_iter()
                .enumerate()
                .map(|(i, item)| {
                    (
                        i,
                        VecSignalItem {
                            id: i,
                            signal: item.into_signal(&config.item),
                        },
                    )
                })
                .collect::<IndexMap<_, _>>(),
        )
    }
    fn try_from_signal(signal: Self::Signal, config: &Self::Config) -> Result<Self, FormError> {
        signal.with(|value| {
            value
                .iter()
                .map(|(_, item)| T::try_from_signal(item.signal.clone(), &config.item))
                .collect()
        })
    }
    fn reset_initial_value(signal: &Self::Signal) {
        signal.update(|sig| {
            sig.iter().for_each(|(_, sig)| T::reset_initial_value(&sig.signal));
        });
    }
    fn with_error<O>(_: &Self::Signal, f: impl FnOnce(Option<&FormError>) -> O) -> O {
        f(None)
    }
}

impl<T, El, S> FormComponent<Vec<El>> for Vec<T>
where
    T: FormComponent<El, Signal = FormFieldSignal<S>>,
    S: Clone + Eq + 'static,
{
    fn render(props: RenderProps<Self::Signal, Self::Config>) -> impl IntoView {
        let (min_items, max_items) = props.config.size.split();

        let next_id =
            create_rw_signal(
                props
                    .signal
                    .with(|items| if items.is_empty() { 1 } else { items[items.len() - 1].id + 1 }),
            );

        if min_items.is_some() || max_items.is_some() {
            props.signal.update(|items| {
                let num_items = items.len();
                if let Some(min_items) = min_items {
                    if items.len() < min_items {
                        items.reserve(min_items - num_items);
                        while items.len() < min_items {
                            let id = next_id.get_untracked();
                            items.insert(
                                id,
                                VecSignalItem {
                                    id,
                                    signal: T::default_signal(),
                                },
                            );
                            next_id.update(|x| *x += 1);
                        }
                    }
                }
                if let Some(max_items) = max_items {
                    while max_items < items.len() {
                        items.pop();
                    }
                }
            });
        }

        let VecConfig {
            item: item_config,
            item_class,
            item_label,
            item_style,
            size,
            add,
            remove,
        } = props.config;

        view! {
            <For
                key=|(_, (key, _))| *key
                each=move || props.signal.get().value.into_iter().enumerate()
                children=move |(index, (key, item))| {
                    let id = || index.to_string();

                    let item_props = RenderProps::builder()
                        .id(Oco::Owned(id()))
                        .name(crate::format_form_name(Some(&props.name), id()))
                        .class(item_class.map(Oco::Borrowed).or_else(|| props.class.clone()))
                        .style(item_style.map(Oco::Borrowed).or_else(|| props.style.clone()))
                        .field_changed_class(props.field_changed_class.clone())
                        .signal(item.signal)
                        .config(item_config.clone())
                        .build();

                    VecConfig::<<T as FormField<El>>::Config>::wrap(
                        &size,
                        item_class,
                        item_label.as_ref(),
                        item_style,
                        &remove,
                        props.signal,
                        key,
                        Oco::Owned(id()),
                        <T as FormComponent<El>>::render(item_props),
                    ).into_view()
                }
            />
            {
                let num_items_is_max = move || {
                    let num_items = props.signal.with(|items| items.len());
                    num_items >= max_items.unwrap_or(usize::MAX)
                };

                let cursor = move || if num_items_is_max() { None } else { Some("pointer") };
                let opacity = move || if num_items_is_max() { Some("0.5") } else { None };

                let on_add = move |_| {
                    if !num_items_is_max() {
                        props.signal.update(|items| {
                            let id = next_id.get_untracked();
                            items.insert(id, VecSignalItem { id, signal: T::default_signal() });
                            next_id.update(|x| *x = id + 1);
                        });
                    }
                };

                match (&size, &add) {
                    (VecConfigSize::Const(_), _)|(_, Adornment::None) => View::default(),
                    (_, Adornment::Component(component)) => component(Rc::new(on_add), Rc::new(opacity)),
                    (_, Adornment::Default) => view! {
                        <input
                            type="button"
                            on:click=on_add
                            style:cursor=cursor
                            style:margin-top="0.5 rem"
                            style:opacity=opacity
                            value="Add"
                        />
                    }
                    .into_view(),
                    (_, Adornment::Spec(adornment_spec)) => {
                        let style = (adornment_spec.class.is_none() && adornment_spec.style.is_none()).then_some("margin-top: 0.5rem;");
                        view! {
                            <input
                                type="button"
                                class={adornment_spec.class.clone()}
                                cursor=cursor
                                on:click=on_add
                                style:opacity=opacity
                                style=style
                                value={adornment_spec.text.clone().unwrap_or(Oco::Borrowed("Add"))}
                            />
                        }
                        .into_view()
                    }
                }
            }
        }
    }
}

impl From<usize> for VecConfigSize {
    fn from(value: usize) -> Self {
        Self::Const(value)
    }
}

impl From<(usize, usize)> for VecConfigSize {
    fn from(value: (usize, usize)) -> Self {
        Self::Bounded {
            min: Some(value.0),
            max: Some(value.1),
        }
    }
}

static ASCII_LOWER: [char; 26] = [
    'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w',
    'x', 'y', 'z',
];

static ASCII_UPPER: [char; 26] = [
    'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W',
    'X', 'Y', 'Z',
];

impl<Config: Default> VecConfig<Config> {
    #[allow(clippy::too_many_arguments)]
    fn wrap<Signal>(
        size: &VecConfigSize,
        item_class: Option<&'static str>,
        item_label: Option<&VecItemLabel>,
        item_style: Option<&'static str>,
        remove_adornment: &Adornment,
        signal: FormFieldSignal<IndexMap<usize, VecSignalItem<Signal>>>,
        key: usize,
        id: Oco<'static, str>,
        item: impl IntoView,
    ) -> impl IntoView {
        static DEFAULT_ITEM_STYLE: &str =
            "display: flex; flex-direction: row; align-items: center; margin-bottom: 0.5rem";

        let (min_items, _) = size.split();
        let num_items_is_min = move || {
            let num_items = signal.with(|items| items.len());
            num_items <= min_items.unwrap_or_default()
        };

        let cursor: StyleSignal = Rc::new(move || if num_items_is_min() { None } else { Some("pointer") });
        let opacity: StyleSignal = Rc::new(move || if num_items_is_min() { Some("0.5") } else { None });

        use leptos::ev::MouseEvent;
        use wasm_bindgen::{JsCast, UnwrapThrowExt};
        let on_remove = move |ev: MouseEvent| {
            if !num_items_is_min() {
                let target = ev.target().unwrap_throw();
                let el = target.unchecked_into::<web_sys::Element>();
                let parent = el.parent_element().unwrap_throw();
                let id = parent.get_attribute("id");
                let Some(id) = id else {
                    logging::debug_warn!("unable to remove component: no id found in parent element");
                    return;
                };
                let key = id.parse::<usize>().unwrap_throw();
                signal.update(|items| {
                    items.remove(&key);
                });
            }
        };

        let remove_component = match (size, remove_adornment) {
            (VecConfigSize::Const(_), _) | (_, Adornment::None) => View::default(),
            (_, Adornment::Component(component)) => component(Rc::new(on_remove), opacity),
            (_, Adornment::Default) => {
                view! {
                    <MaterialClose
                        cursor=cursor
                        on:click=on_remove
                        opacity=opacity
                        style=Oco::Borrowed("margin-left: 0.5rem !important;")
                    />
                }
            }
            (_, Adornment::Spec(adornment_spec)) => {
                let adornment_style = (adornment_spec.class.is_none() && adornment_spec.style.is_none())
                    .then_some("margin-left: 0.5rem;");
                view! {
                    <MaterialClose
                        class=adornment_spec.class.clone()
                        cursor=cursor
                        height=adornment_spec.height
                        on:click=on_remove
                        opacity=opacity
                        style=adornment_style.map(Oco::from)
                        width=adornment_spec.width
                    />
                }
            }
        };

        let item_style = item_style.or_else(|| item_class.is_none().then_some(DEFAULT_ITEM_STYLE));

        view! {
            <div id={key} class={item_class} style={item_style}>
                {match item_label {
                    Some(item_label) => item_label.wrap_label(key, id, item, signal),
                    None => item.into_view(),
                }}
                {remove_component}
            </div>
        }
    }
}

impl VecItemLabel {
    fn wrap_label<Signal>(
        &self,
        key: usize,
        id: Oco<'static, str>,
        item: impl IntoView,
        signal: FormFieldSignal<IndexMap<usize, VecSignalItem<Signal>>>,
    ) -> View {
        let notation = self.notation;
        let notation = move || {
            let index = signal.with(|items| items.get_index_of(&key));
            index.map(|index| notation.render(index))
        };
        view! {
            <label for={id} class={self.class} style={self.style}>
                <div>{notation}{self.punctuation.render()}</div>
                {item}
            </label>
        }
        .into_view()
    }
}

impl VecConfigSize {
    fn split(&self) -> (Option<usize>, Option<usize>) {
        match *self {
            VecConfigSize::Bounded { min, max } => (min, max),
            VecConfigSize::Const(num_items) => (Some(num_items), Some(num_items)),
            VecConfigSize::Unbounded => (None, None),
        }
    }
}

impl VecItemLabelNotation {
    fn render(&self, i: usize) -> String {
        let ascii_set = match self {
            Self::CapitalLetter => &ASCII_UPPER,
            Self::Letter => &ASCII_LOWER,
            Self::Number => return (i + 1).to_string(),
        };
        let n = (i.ilog(ascii_set.len()) + 1) as usize;
        let mut chars = Vec::with_capacity(n);
        let mut num = i;
        for j in 0..n {
            chars[n - 1 - j] = ascii_set[num % ascii_set.len()];
            num /= ascii_set.len();
        }
        chars.into_iter().collect()
    }
}

impl VecItemLabelPunctuation {
    fn render(&self) -> &'static str {
        match self {
            Self::None => "",
            Self::Parenthesis => ")",
            Self::Period => ".",
        }
    }
}
