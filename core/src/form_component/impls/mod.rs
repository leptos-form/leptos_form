mod misc;
mod num;
mod str;

/// Field configuration utilities
pub mod config {
    pub use super::misc::*;

    /// Configuration utilities for using collections in form types.
    pub mod collections {
        include!("./collections.rs");
    }
}
