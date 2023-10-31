```rust
#[derive(leptos_form::Form)]
#[form(component)]
pub struct MyForm {
    pub id: String,
}

#[leptos::component]
pub fn AnotherComponent() -> impl leptos::IntoView {
    let initial = MyForm { id: "1".to_string() };
    view! {
        <MyForm initial={initial} />
    }
}
```
