pub mod config;
pub mod nif_methods;
pub mod s3;
use crate::nif_methods::{
    create_bucket, delete_object, delete_objects, get_cached_object, get_object, head_bucket,
    head_object, list_objects, put_object,
};

rustler::init!(
    "s3_nif",
    [
        get_object,
        put_object,
        create_bucket,
        head_object,
        delete_object,
        head_bucket,
        list_objects,
        delete_objects,
        get_cached_object
    ]
);
