pub mod config;
pub mod nif_methods;
pub mod s3;
pub mod server;
use crate::nif_methods::{create_bucket, delete_object, get_object, head_object, put_object};

rustler::init!(
    "s3_nif",
    [
        get_object,
        put_object,
        create_bucket,
        head_object,
        delete_object
    ]
);
