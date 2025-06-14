pub mod core;
// imports
use rustler::{Nif, NifResult, Error};
use crate::core::arweave::query_permaweb;

mod atoms {
    rustler::atoms! {
        ok,
    }
}

#[rustler::nif]
fn hello() -> NifResult<String> {
    Ok("Hello world!".to_string())
}

#[rustler::nif(schedule = "DirtyCpu")]
fn query() -> NifResult<String> {
    query_permaweb().map_err(|err| Error::Term(Box::new(err.to_string())))
}

// update this adding `query()` function
rustler::init!("my_device", [hello, query]);

/// tests

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_query_permaweb() {
        let id = query_permaweb().unwrap();
        println!("TXID: {:?}", id);
        assert_eq!(id.len(), 43);
    }
}