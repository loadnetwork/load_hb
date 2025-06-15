use alloy::{
    eips::BlockId,
    primitives::{Address, B256, Bytes, U256},
};
use anyhow::Error;
use serde_json::Value;
use std::str::FromStr;

pub fn parse_block_id(value: &Value) -> Result<BlockId, Error> {
    match value {
        Value::String(s) => {
            if s == "latest" {
                Ok(BlockId::latest())
            } else if s == "earliest" {
                Ok(BlockId::earliest())
            } else if s == "pending" {
                Ok(BlockId::pending())
            } else if s == "safe" {
                Ok(BlockId::safe())
            } else if s == "finalized" {
                Ok(BlockId::finalized())
            } else if s.starts_with("0x") {
                // Try to parse as block hash
                let hash = B256::from_str(s)
                    .map_err(|e| Error::msg(format!("Invalid block hash: {}", e)))?;
                Ok(BlockId::Hash(hash.into()))
            } else {
                // Try to parse as block number
                let num = s
                    .parse::<u64>()
                    .map_err(|e| Error::msg(format!("Invalid block number: {}", e)))?;
                Ok(BlockId::Number(num.into()))
            }
        }
        Value::Number(n) => {
            if let Some(num) = n.as_u64() {
                Ok(BlockId::Number(num.into()))
            } else {
                Err(Error::msg("Invalid block number"))
            }
        }
        _ => Err(Error::msg("Invalid block identifier")),
    }
}

pub fn parse_address(value: &Value) -> Result<Address, Error> {
    match value {
        Value::String(s) => {
            Address::from_str(s).map_err(|e| Error::msg(format!("Invalid address: {}", e)))
        }
        _ => Err(Error::msg("Invalid address format")),
    }
}

pub fn parse_b256(value: &Value) -> Result<B256, Error> {
    match value {
        Value::String(s) => {
            B256::from_str(s).map_err(|e| Error::msg(format!("Invalid hash: {}", e)))
        }
        _ => Err(Error::msg("Invalid hash format")),
    }
}

pub fn parse_u256(value: &Value) -> Result<U256, Error> {
    match value {
        Value::String(s) => {
            U256::from_str(s).map_err(|e| Error::msg(format!("Invalid U256: {}", e)))
        }
        Value::Number(n) => {
            if let Some(num) = n.as_u64() {
                Ok(U256::from(num))
            } else {
                Err(Error::msg("Invalid U256 number"))
            }
        }
        _ => Err(Error::msg("Invalid U256 format")),
    }
}

pub fn parse_bytes(value: &Value) -> Result<Bytes, Error> {
    match value {
        Value::String(s) => {
            if s.starts_with("0x") {
                let s = s.trim_start_matches("0x");
                let bytes =
                    hex::decode(s).map_err(|e| Error::msg(format!("Invalid hex: {}", e)))?;
                Ok(Bytes::from(bytes))
            } else {
                Err(Error::msg("Hex string must start with 0x"))
            }
        }
        _ => Err(Error::msg("Invalid bytes format")),
    }
}
