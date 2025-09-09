use crate::sidecar::tags::extract_mime_from_tags;
use byteorder::{LittleEndian, ReadBytesExt};
use std::io::Cursor;

/// Parse ANS-104 dataitem header (Arweave only).
///
/// Assumes:
/// - `signature_type == 1` (Arweave/RSA-2048).
/// - signature length = 512 bytes.
/// - owner/public key length = 512 bytes.
///
/// Skips past signature + owner, optional target/anchor,
/// then reads the tags block (count + length).
/// Extracts the MIME type from tags if present.
///
/// Will fail for non-Arweave signed dataitems
pub(crate) fn parse_ans104_header(header: &[u8]) -> Result<(String, usize), String> {
    let mut cursor = Cursor::new(header);

    // u16 LE signature_type (should be 1 for Arweave)
    let sig_type = cursor
        .read_u16::<LittleEndian>()
        .map_err(|_| "read signature_type failed")?;
    if sig_type != 1 {
        return Err("expected Arweave signer type 1".into());
    }

    // skip signature (512) + owner (512)
    cursor.set_position(cursor.position() + 1024);

    // target presence (u8) + optional 32 bytes
    if cursor.read_u8().map_err(|_| "read target_present failed")? == 1 {
        cursor.set_position(cursor.position() + 32);
    }

    // anchor presence (u8) + optional 32 bytes
    if cursor.read_u8().map_err(|_| "read anchor_present failed")? == 1 {
        cursor.set_position(cursor.position() + 32);
    }

    // tags: u64 LE count, u64 LE bytes_len
    let _tag_count = cursor
        .read_u64::<LittleEndian>()
        .map_err(|_| "read tag_count failed")?;
    let tag_bytes_len = cursor
        .read_u64::<LittleEndian>()
        .map_err(|_| "read tag_bytes_len failed")? as usize;
    let tag_start = cursor.position() as usize;
    let tag_end = tag_start + tag_bytes_len;
    if tag_end > header.len() {
        return Err("header too small for tags".into());
    }

    let mime = extract_mime_from_tags(&header[tag_start..tag_end]);

    Ok((mime, tag_end))
}
