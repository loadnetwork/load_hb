mod create_bucket_nif;
mod get_object_nif;
mod put_object_nif;

pub use create_bucket_nif::create_bucket;
pub use get_object_nif::get_object;
pub use put_object_nif::put_object;
