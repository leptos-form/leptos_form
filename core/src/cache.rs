use ::core::future::Future;

#[cfg(feature = "serde")]
use ::serde::{de::DeserializeOwned, Serialize};

pub trait Cache<CS: CacheSerializer<T>, T> {
    type Error: Send;
    fn get_item(&self, key: &str) -> impl Future<Output = Result<Option<T>, Self::Error>> + Send;
    fn remove_item(&self, key: &str) -> impl Future<Output = Result<(), Self::Error>> + Send;
    fn set_item(&self, key: &str, value: &T) -> impl Future<Output = Result<(), Self::Error>> + Send;
}

pub trait CacheSerializer<T> {
    type Serialized;
    type Error: Send;
    fn serialize(value: &T) -> Result<Self::Serialized, Self::Error>;
    fn deserialize(serialized: &Self::Serialized) -> Result<T, Self::Error>;
}

#[cfg(feature = "cache-local-storage")]
pub struct LocalStorage<T>(pub T);

#[cfg(feature = "cache-serde_json")]
pub struct SerdeJson;

#[cfg(feature = "serde")]
pub trait SerdeSerializer {
    type Serialized;
    type Error: Send;
    fn ser<T: Serialize>(value: &T) -> Result<Self::Serialized, Self::Error>;
    fn de<T: DeserializeOwned>(value: &Self::Serialized) -> Result<T, Self::Error>;
}

#[cfg(feature = "serde")]
impl<S: SerdeSerializer, T: DeserializeOwned + Serialize> CacheSerializer<T> for S {
    type Serialized = S::Serialized;
    type Error = S::Error;
    fn serialize(value: &T) -> Result<Self::Serialized, Self::Error> {
        S::ser(value)
    }
    fn deserialize(serialized: &Self::Serialized) -> Result<T, Self::Error> {
        S::de(serialized)
    }
}

#[cfg(feature = "cache-serde_json")]
impl SerdeSerializer for SerdeJson {
    type Serialized = String;
    type Error = serde_json::Error;
    fn ser<T: Serialize>(value: &T) -> Result<Self::Serialized, Self::Error> {
        serde_json::to_string(value)
    }
    fn de<T: DeserializeOwned>(serialized: &Self::Serialized) -> Result<T, Self::Error> {
        serde_json::from_str(serialized)
    }
}

#[cfg(feature = "cache-local-storage")]
impl<CS: CacheSerializer<T, Serialized = String>, T> Cache<CS, T> for LocalStorage<CS> {
    type Error = CS::Error;
    fn get_item(&self, key: &str) -> impl Future<Output = Result<Option<T>, CS::Error>> + Send {
        use wasm_bindgen::UnwrapThrowExt;
        let window = web_sys::window().unwrap_throw();
        let local_storage = window.local_storage().unwrap_throw().unwrap_throw();
        let serialized = local_storage.get_item(key).unwrap_throw();
        async move {
            let serialized = match serialized {
                Some(serialized) => serialized,
                None => return Ok(None),
            };
            CS::deserialize(&serialized).map(Some)
        }
    }
    fn remove_item(&self, key: &str) -> impl Future<Output = Result<(), CS::Error>> + Send {
        use wasm_bindgen::UnwrapThrowExt;
        let window = web_sys::window().unwrap_throw();
        let local_storage = window.local_storage().unwrap_throw().unwrap_throw();
        local_storage.remove_item(key).unwrap_throw();
        async move { Ok(()) }
    }
    fn set_item(&self, key: &str, value: &T) -> impl Future<Output = Result<(), CS::Error>> + Send {
        use wasm_bindgen::UnwrapThrowExt;
        let window = web_sys::window().unwrap_throw();
        let local_storage = window.local_storage().unwrap_throw().unwrap_throw();
        let res = CS::serialize(value).map(|serialized| local_storage.set_item(key, &serialized).unwrap_throw());
        async move { res }
    }
}
