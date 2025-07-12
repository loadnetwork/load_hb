mod create_bucket;
mod create_client;
mod push_object;
mod retrieve_object;

pub use create_bucket::create_bucket;
pub use create_client::create_s3_client;
pub use push_object::push_object;
pub use retrieve_object::retrieve_object;

use crate::server::S3Error;
use aws_sdk_s3::Client;
use lru::LruCache;
use parking_lot::RwLock;
use std::num::NonZeroUsize;

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
    let response = retrieve_object(client, bucket_name, key).await?;
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

pub async fn push_object_with_metadata(
    client: &Client,
    bucket_name: &str,
    key: &str,
    file_data: impl AsRef<[u8]>,
    file_content_type: Option<&str>,
    create_bucket_if_does_not_exist: bool,
) -> Result<(Vec<u8>, Option<String>), S3Error> {
    // Create bucket if requested and it doesn't exist
    if create_bucket_if_does_not_exist {
        if let Err(_) = create_bucket(client, bucket_name).await {
            // Bucket might already exist, continue
        }
    }

    // Push the object first
    let body =
        aws_sdk_s3::primitives::ByteStream::from(bytes::Bytes::copy_from_slice(file_data.as_ref()));

    let mut put_request = client.put_object().bucket(bucket_name).key(key).body(body);

    if let Some(content_type) = file_content_type {
        put_request = put_request.content_type(content_type);
    }

    put_request
        .send()
        .await
        .map_err(|e| S3Error::Internal(e.to_string()))?;

    // Retrieve the object after pushing to get metadata
    let response = retrieve_object(client, bucket_name, key).await?;
    let etag = response.e_tag().map(|s| s.to_string());
    let bytes = response
        .body
        .collect()
        .await
        .map_err(|e| S3Error::Internal(e.to_string()))?;

    let data = bytes.into_bytes().to_vec();

    // Note: We don't add to cache here as requested

    Ok((data, etag))
}
