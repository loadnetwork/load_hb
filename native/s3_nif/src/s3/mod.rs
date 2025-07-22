mod create_bucket;
mod create_client;
mod delete_object;
mod delete_objects;
mod errors;
mod get_object;
mod get_object_presigned_url;
mod head_bucket;
mod head_object;
mod list_objects;
mod put_object;
mod put_object_expiry;

pub use create_bucket::create_bucket;
pub use create_client::create_s3_client;
pub use delete_object::delete_object;
pub use delete_objects::delete_objects;
pub use get_object::get_object;
pub use get_object_presigned_url::generate_get_presigned_url;
pub use head_bucket::head_bucket;
pub use head_object::head_object;
pub use list_objects::list_objects;
pub use put_object::put_object;
pub use put_object_expiry::put_object_expiry;

use aws_sdk_s3::Client;
pub use errors::S3Error;
use lru::LruCache;
use parking_lot::RwLock;
use std::num::NonZeroUsize;
// constants
pub const MIN_OBJECT_EXPIRY_DAYS: i32 = 1;
pub const MAX_OBJECT_EXPIRY_DAYS: i32 = 365;
pub const MIN_PRESIGNED_URL_DURATION: u64 = 1; //  1 sec
pub const MAX_PRESIGNED_URL_DURATION: u64 = 7 * 24 * 3600; // 7 days


// Cache entry with size tracking
#[derive(Clone)]
struct CacheEntry {
    data: Vec<u8>,
    etag: Option<String>,
    size: usize,
}

// LRU cache with size-based eviction
struct SizedLruCache {
    cache: LruCache<String, CacheEntry>,
    current_size: usize,
    max_size: usize,
}

impl SizedLruCache {
    fn new(max_size: usize) -> Self {
        Self {
            cache: LruCache::new(NonZeroUsize::new(10000).unwrap()),
            current_size: 0,
            max_size,
        }
    }

    fn get(&mut self, key: &str) -> Option<&CacheEntry> {
        self.cache.get(key)
    }

    fn put(&mut self, key: String, entry: CacheEntry) {
        // Remove entries until we have space
        while self.current_size + entry.size > self.max_size {
            if let Some((_, old_entry)) = self.cache.pop_lru() {
                self.current_size -= old_entry.size;
            } else {
                break;
            }
        }

        // Only add if it fits
        if entry.size <= self.max_size {
            self.current_size += entry.size;
            self.cache.put(key, entry);
        }
    }
}

fn get_cache_size() -> usize {
    std::env::var("S3_CACHE_SIZE_GB")
        .ok()
        .and_then(|s| s.parse::<usize>().ok())
        .unwrap_or(2)
        * 1024
        * 1024
        * 1024
}

// Static LRU cache for object metadata
static OBJECT_CACHE: std::sync::LazyLock<RwLock<SizedLruCache>> =
    std::sync::LazyLock::new(|| RwLock::new(SizedLruCache::new(get_cache_size())));

pub async fn get_object_with_metadata(
    client: &Client,
    bucket_name: &str,
    key: &str,
    range: &str,
) -> Result<(Vec<u8>, Option<String>), S3Error> {
    let cache_key = format!("{}:{}", bucket_name, key);

    // Try to get from cache first (using write lock for proper LRU behavior)
    {
        let mut cache = OBJECT_CACHE.write();
        if let Some(entry) = cache.get(&cache_key) {
            return Ok((entry.data.clone(), entry.etag.clone()));
        }
    }

    // Not in cache, fetch from S3
    let response = get_object(client, bucket_name, key, range).await?;
    let etag = response.e_tag().map(|s| s.to_string());
    let bytes = response
        .body
        .collect()
        .await
        .map_err(|e| S3Error::Internal(e.to_string()))?;

    let data = bytes.into_bytes().to_vec();

    // Store in cache
    {
        let mut cache = OBJECT_CACHE.write();
        let entry = CacheEntry {
            size: data.len(),
            data: data.clone(),
            etag: etag.clone(),
        };
        cache.put(cache_key, entry);
    }

    Ok((data, etag))
}
