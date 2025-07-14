mod create_bucket_nif;
mod delete_object_nif;
mod get_object_nif;
mod head_bucket_nif;
mod head_object_nif;
mod put_object_nif;
mod list_objects_nif;
mod list_all_objects_nif;

pub use create_bucket_nif::create_bucket;
pub use delete_object_nif::delete_object;
pub use get_object_nif::get_object;
pub use head_bucket_nif::head_bucket;
pub use head_object_nif::head_object;
pub use put_object_nif::put_object;
pub use list_objects_nif::list_objects;
pub use list_all_objects_nif::list_all_objects;
