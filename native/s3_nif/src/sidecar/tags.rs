use bundles_rs::ans104::tags::{Tag, decode_tags};

pub(crate) fn extract_mime_from_tags(tag_bytes: &[u8]) -> String {
    // parse ANS-104 Avro tags
    if let Ok(tags) = decode_tags(tag_bytes) {
        if let Some(m) = find_mime_in_tags(&tags) {
            println!("Found MIME type in Avro tags: {}", m);
            return m;
        }
    }

    "application/octet-stream".to_string()
}

pub fn find_mime_in_tags(tags: &[Tag]) -> Option<String> {
    for t in tags {
        if t.name.trim().eq_ignore_ascii_case("content-type") {
            return Some(t.value.clone())
        }
    }
    None
}
