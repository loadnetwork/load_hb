use alloy::eips::BlockId;
use anyhow::Error;
use hyper::{Body, Method, Request, Response, StatusCode};
use serde_json::{Value, json};
use std::convert::Infallible;
use std::sync::{Arc, Mutex};
use tracing::{error, info};

use crate::core::server::create_error_response;
use crate::core::types::{
    JsonRpcError, JsonRpcErrorResponse, JsonRpcRequest, JsonRpcResponse, ServerState,
};
use crate::core::utils::{parse_address, parse_b256, parse_block_id, parse_bytes, parse_u256};

pub async fn handle_request(
    req: Request<Body>,
    state: Arc<Mutex<ServerState>>,
) -> Result<Response<Body>, Infallible> {
    let mut response = Response::new(Body::empty());

    if req.method() == Method::POST && (req.uri().path() == "/" || req.uri().path() == "") {
        let whole_body = match hyper::body::to_bytes(req.into_body()).await {
            Ok(body) => body,
            Err(e) => {
                *response.status_mut() = StatusCode::BAD_REQUEST;
                *response.body_mut() = Body::from(format!("Error reading request body: {}", e));
                return Ok(response);
            }
        };

        let rpc_req: JsonRpcRequest = match serde_json::from_slice(&whole_body) {
            Ok(req) => req,
            Err(e) => {
                let error_response = JsonRpcErrorResponse {
                    jsonrpc: "2.0".to_string(),
                    error: JsonRpcError {
                        code: -32700,
                        message: format!("Parse error: {}", e),
                    },
                    id: Value::Null,
                };
                *response.status_mut() = StatusCode::BAD_REQUEST;
                *response.body_mut() = Body::from(serde_json::to_string(&error_response).unwrap());
                response.headers_mut().insert(
                    hyper::header::CONTENT_TYPE,
                    hyper::header::HeaderValue::from_static("application/json"),
                );
                return Ok(response);
            }
        };

        info!("Received JSON-RPC request: method={}", rpc_req.method);

        let client = {
            let state_guard = state.lock().unwrap();
            state_guard.client.clone()
        };

        let method_result = match rpc_req.method.as_str() {
            "eth_blockNumber" => match client.get_block_number().await {
                Ok(v) => Ok(json!(v)),
                Err(e) => Err(Error::msg(e.to_string())),
            },
            "eth_getBlockByNumber" => {
                if rpc_req.params.len() < 2 {
                    Err(Error::msg("Invalid params for eth_getBlockByNumber"))
                } else {
                    let block_id = match parse_block_id(&rpc_req.params[0]) {
                        Ok(id) => id,
                        Err(e) => return create_error_response(-32602, &e.to_string(), rpc_req.id),
                    };

                    let full_tx = match &rpc_req.params[1] {
                        Value::Bool(b) => *b,
                        _ => {
                            return create_error_response(
                                -32602,
                                "Invalid full_tx parameter",
                                rpc_req.id,
                            );
                        }
                    };

                    match client.get_block(block_id, full_tx).await {
                        Ok(v) => Ok(json!(v)),
                        Err(e) => Err(Error::msg(e.to_string())),
                    }
                }
            }
            "eth_getBlockByHash" => {
                if rpc_req.params.len() < 2 {
                    Err(Error::msg("Invalid params for eth_getBlockByHash"))
                } else {
                    let hash = match parse_b256(&rpc_req.params[0]) {
                        Ok(h) => h,
                        Err(e) => return create_error_response(-32602, &e.to_string(), rpc_req.id),
                    };

                    let full_tx = match &rpc_req.params[1] {
                        Value::Bool(b) => *b,
                        _ => {
                            return create_error_response(
                                -32602,
                                "Invalid full_tx parameter",
                                rpc_req.id,
                            );
                        }
                    };

                    match client.get_block(BlockId::Hash(hash.into()), full_tx).await {
                        Ok(v) => Ok(json!(v)),
                        Err(e) => Err(Error::msg(e.to_string())),
                    }
                }
            }
            "eth_getTransactionByHash" => {
                if rpc_req.params.is_empty() {
                    Err(Error::msg("Invalid params for eth_getTransactionByHash"))
                } else {
                    let hash = match parse_b256(&rpc_req.params[0]) {
                        Ok(h) => h,
                        Err(e) => return create_error_response(-32602, &e.to_string(), rpc_req.id),
                    };

                    match client.get_transaction(hash).await {
                        Ok(v) => Ok(json!(v)),
                        Err(e) => Err(Error::msg(e.to_string())),
                    }
                }
            }
            "eth_getTransactionReceipt" => {
                if rpc_req.params.is_empty() {
                    Err(Error::msg("Invalid params for eth_getTransactionReceipt"))
                } else {
                    let hash = match parse_b256(&rpc_req.params[0]) {
                        Ok(h) => h,
                        Err(e) => return create_error_response(-32602, &e.to_string(), rpc_req.id),
                    };

                    match client.get_transaction_receipt(hash).await {
                        Ok(v) => Ok(json!(v)),
                        Err(e) => Err(Error::msg(e.to_string())),
                    }
                }
            }
            "eth_sendRawTransaction" => {
                if rpc_req.params.is_empty() {
                    Err(Error::msg("Invalid params for eth_sendRawTransaction"))
                } else {
                    let bytes = match parse_bytes(&rpc_req.params[0]) {
                        Ok(b) => b,
                        Err(e) => return create_error_response(-32602, &e.to_string(), rpc_req.id),
                    };

                    match client.send_raw_transaction(&bytes).await {
                        Ok(v) => Ok(json!(v)),
                        Err(e) => Err(Error::msg(e.to_string())),
                    }
                }
            }
            "eth_getBalance" => {
                if rpc_req.params.len() < 2 {
                    Err(Error::msg("Invalid params for eth_getBalance"))
                } else {
                    let address = match parse_address(&rpc_req.params[0]) {
                        Ok(a) => a,
                        Err(e) => return create_error_response(-32602, &e.to_string(), rpc_req.id),
                    };

                    let block_id = match parse_block_id(&rpc_req.params[1]) {
                        Ok(id) => id,
                        Err(e) => return create_error_response(-32602, &e.to_string(), rpc_req.id),
                    };

                    match client.get_balance(address, block_id).await {
                        Ok(v) => Ok(json!(v)),
                        Err(e) => Err(Error::msg(e.to_string())),
                    }
                }
            }
            "eth_getCode" => {
                if rpc_req.params.len() < 2 {
                    Err(Error::msg("Invalid params for eth_getCode"))
                } else {
                    let address = match parse_address(&rpc_req.params[0]) {
                        Ok(a) => a,
                        Err(e) => return create_error_response(-32602, &e.to_string(), rpc_req.id),
                    };

                    let block_id = match parse_block_id(&rpc_req.params[1]) {
                        Ok(id) => id,
                        Err(e) => return create_error_response(-32602, &e.to_string(), rpc_req.id),
                    };

                    match client.get_code(address, block_id).await {
                        Ok(v) => Ok(json!(v)),
                        Err(e) => Err(Error::msg(e.to_string())),
                    }
                }
            }
            "eth_getTransactionCount" => {
                if rpc_req.params.len() < 2 {
                    Err(Error::msg("Invalid params for eth_getTransactionCount"))
                } else {
                    let address = match parse_address(&rpc_req.params[0]) {
                        Ok(a) => a,
                        Err(e) => return create_error_response(-32602, &e.to_string(), rpc_req.id),
                    };

                    let block_id = match parse_block_id(&rpc_req.params[1]) {
                        Ok(id) => id,
                        Err(e) => return create_error_response(-32602, &e.to_string(), rpc_req.id),
                    };

                    match client.get_nonce(address, block_id).await {
                        Ok(v) => Ok(json!(v)),
                        Err(e) => Err(Error::msg(e.to_string())),
                    }
                }
            }
            "eth_getStorageAt" => {
                if rpc_req.params.len() < 3 {
                    Err(Error::msg("Invalid params for eth_getStorageAt"))
                } else {
                    let address = match parse_address(&rpc_req.params[0]) {
                        Ok(a) => a,
                        Err(e) => return create_error_response(-32602, &e.to_string(), rpc_req.id),
                    };

                    let slot = match parse_u256(&rpc_req.params[1]) {
                        Ok(s) => s,
                        Err(e) => return create_error_response(-32602, &e.to_string(), rpc_req.id),
                    };

                    let block_id = match parse_block_id(&rpc_req.params[2]) {
                        Ok(id) => id,
                        Err(e) => return create_error_response(-32602, &e.to_string(), rpc_req.id),
                    };

                    match client.get_storage_at(address, slot, block_id).await {
                        Ok(v) => Ok(json!(v)),
                        Err(e) => Err(Error::msg(e.to_string())),
                    }
                }
            }
            "eth_chainId" => match Ok::<u64, Error>(client.get_chain_id().await) {
                Ok(v) => Ok(json!(v)),
                Err(e) => Err(Error::msg(e.to_string())),
            },
            "eth_gasPrice" => match client.get_gas_price().await {
                Ok(v) => Ok(json!(v)),
                Err(e) => Err(Error::msg(e.to_string())),
            },
            "eth_maxPriorityFeePerGas" => match client.get_priority_fee().await {
                Ok(v) => Ok(json!(v)),
                Err(e) => Err(Error::msg(e.to_string())),
            },
            "eth_blobBaseFee" => match client.get_blob_base_fee().await {
                Ok(v) => Ok(json!(v)),
                Err(e) => Err(Error::msg(e.to_string())),
            },
            "eth_syncing" => match client.syncing().await {
                Ok(v) => Ok(json!(v)),
                Err(e) => Err(Error::msg(e.to_string())),
            },
            "eth_coinbase" => match client.get_coinbase().await {
                Ok(v) => Ok(json!(v)),
                Err(e) => Err(Error::msg(e.to_string())),
            },
            "web3_clientVersion" => Ok(json!(client.get_client_version().await)),
            _ => Err(Error::msg(format!(
                "Method {} not supported",
                rpc_req.method
            ))),
        };

        match method_result {
            Ok(result) => {
                let json_response = JsonRpcResponse {
                    jsonrpc: "2.0".to_string(),
                    result,
                    id: rpc_req.id,
                };
                *response.body_mut() = Body::from(serde_json::to_string(&json_response).unwrap());
            }
            Err(e) => {
                error!("Error executing method {}: {}", rpc_req.method, e);
                let error_response = JsonRpcErrorResponse {
                    jsonrpc: "2.0".to_string(),
                    error: JsonRpcError {
                        code: -32603,
                        message: format!("Internal error: {}", e),
                    },
                    id: rpc_req.id,
                };
                *response.status_mut() = StatusCode::INTERNAL_SERVER_ERROR;
                *response.body_mut() = Body::from(serde_json::to_string(&error_response).unwrap());
            }
        }

        response.headers_mut().insert(
            hyper::header::CONTENT_TYPE,
            hyper::header::HeaderValue::from_static("application/json"),
        );
    } else if req.method() == Method::GET && req.uri().path() == "/health" {
        *response.body_mut() = Body::from("OK");
    } else {
        *response.status_mut() = StatusCode::NOT_FOUND;
        *response.body_mut() = Body::from("Not Found");
    }

    Ok(response)
}
