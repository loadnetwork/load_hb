use crate::sidecar::tags::extract_mime_from_tags;
use byteorder::{LittleEndian, ReadBytesExt};
use std::io::Cursor;

// type 1
const ARWEAVE_SIG_BYTES: u64 = 512;
const ARWEAVE_OWNER_BYTES: u64 = 512;
// type 2
const SOL_SIG_BYTES: u64 = 64;
const SOL_OWNER_BYTES: u64 = 32;
// type 3
const ETH_SIG_BYTES: u64 = 65;
const ETH_OWNER_BYTES: u64 = 65;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum DataItemSignatureType {
    Arweave = 1,
    Ed25519 = 2,
    Ethereum = 3,
}

impl DataItemSignatureType {
    pub fn from_u16(value: u16) -> Option<Self> {
        match value {
            1 => Some(DataItemSignatureType::Arweave),
            2 => Some(DataItemSignatureType::Ed25519),
            3 => Some(DataItemSignatureType::Ethereum),
            _ => None,
        }
    }

    pub fn byte_values(self) -> u64 {
        match self {
            DataItemSignatureType::Arweave => ARWEAVE_SIG_BYTES + ARWEAVE_OWNER_BYTES,
            DataItemSignatureType::Ed25519 => SOL_SIG_BYTES + SOL_OWNER_BYTES,
            DataItemSignatureType::Ethereum => ETH_SIG_BYTES + ETH_OWNER_BYTES,
        }
    }
}

/// Parse ANS-104 dataitem header (Arweave, ETH, SOL).
///
/// Skips past signature + owner, optional target/anchor,
/// then reads the tags block (count + length).
/// Extracts the MIME type from tags if present.
///
/// Will fail for non-Arweave signed dataitems
pub(crate) fn parse_ans104_header(header: &[u8]) -> Result<(String, usize), String> {
    let mut cursor = Cursor::new(header);

    // u16 LE signature_type
    let sig_type = cursor
        .read_u16::<LittleEndian>()
        .map_err(|_| "read signature_type failed")?;

    let di_sig_type = DataItemSignatureType::from_u16(sig_type)
        .ok_or_else(|| "error: invalid dataitem signature type".to_string())?;
    let sig_byte_values = di_sig_type.byte_values();

    // skip signature + owner
    cursor.set_position(cursor.position() + sig_byte_values);

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
