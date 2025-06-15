use alloy::{
    eips::BlockId,
    primitives::{Address, B256, Bytes, U256},
    rpc::types::{Filter, Log},
};
use anyhow::Error;
use helios::ethereum::{EthereumClientBuilder, config::networks::Network};
use hyper::{
    Body, Method, Request, Response, Server, StatusCode,
    service::{make_service_fn, service_fn},
};
use serde::{Deserialize, Serialize};
use serde_json::{Value, json};
use std::convert::Infallible;
use std::net::SocketAddr;
use std::path::PathBuf;
use std::str::FromStr;
use std::sync::{Arc, Mutex};
use tokio::runtime::Runtime;
use tokio::sync::oneshot;
use tracing::{error, info};
use tracing_subscriber::FmtSubscriber;
use tracing_subscriber::filter::{EnvFilter, LevelFilter};

use crate::core::types::{JsonRpcRequest, JsonRpcError, JsonRpcErrorResponse, JsonRpcResponse, ServerState};


fn parse_block_id(value: &Value) -> Result<BlockId, Error> {
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

fn parse_address(value: &Value) -> Result<Address, Error> {
    match value {
        Value::String(s) => {
            Address::from_str(s).map_err(|e| Error::msg(format!("Invalid address: {}", e)))
        }
        _ => Err(Error::msg("Invalid address format")),
    }
}

fn parse_b256(value: &Value) -> Result<B256, Error> {
    match value {
        Value::String(s) => {
            B256::from_str(s).map_err(|e| Error::msg(format!("Invalid hash: {}", e)))
        }
        _ => Err(Error::msg("Invalid hash format")),
    }
}

fn parse_u256(value: &Value) -> Result<U256, Error> {
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

fn parse_bytes(value: &Value) -> Result<Bytes, Error> {
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

async fn handle_request(
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

fn create_error_response(
    code: i32,
    message: &str,
    id: Value,
) -> Result<Response<Body>, Infallible> {
    let error_response = JsonRpcErrorResponse {
        jsonrpc: "2.0".to_string(),
        error: JsonRpcError {
            code,
            message: message.to_string(),
        },
        id,
    };

    let mut response = Response::new(Body::from(serde_json::to_string(&error_response).unwrap()));
    response.headers_mut().insert(
        hyper::header::CONTENT_TYPE,
        hyper::header::HeaderValue::from_static("application/json"),
    );

    Ok(response)
}

pub fn start_server(
    address: SocketAddr,
    execution_rpc: String,
    consensus_rpc: String,
    data_dir: PathBuf,
    shutdown_rx: oneshot::Receiver<()>,
) -> Result<Runtime, Error> {
    let env_filter = EnvFilter::builder()
        .with_default_directive(LevelFilter::INFO.into())
        .from_env_lossy();

    let subscriber = FmtSubscriber::builder()
        .with_env_filter(env_filter)
        .finish();

    let _ = tracing::subscriber::set_global_default(subscriber);

    let runtime = Runtime::new()?;

    let client = runtime.block_on(async {
        info!(target: "helios::server", "Building Helios client...");

        let client = EthereumClientBuilder::new()
            .network(Network::Mainnet)
            .execution_rpc(&execution_rpc)
            .unwrap()
            .consensus_rpc(&consensus_rpc)
            .unwrap()
            .data_dir(data_dir)
            .with_file_db()
            .build()
            .unwrap();

        info!(target: "helios::server", "Helios client built successfully");

        Ok::<_, Error>(client)
    })?;

    let state = Arc::new(Mutex::new(ServerState {
        client: Arc::new(client),
        shutdown_signal: Some(shutdown_rx),
    }));

    let state_clone = state.clone();

    runtime.spawn(async move {
        info!(target: "helios::server", "Starting HTTP server on {}", address);
        
        let make_svc = make_service_fn(move |_conn| {
            let state = state_clone.clone();
            async move {
                Ok::<_, Infallible>(service_fn(move |req| {
                    handle_request(req, state.clone())
                }))
            }
        });
        
        let server = Server::bind(&address).serve(make_svc);
        
        let shutdown_signal = {
            let mut state = state.lock().unwrap();
            state.shutdown_signal.take().unwrap()
        };
        
        let server = server.with_graceful_shutdown(async {
            shutdown_signal.await.ok();
            info!(target: "helios::server", "Server shutting down gracefully");
        });
        
        info!(target: "helios::server", "HTTP server running at {}", address);
        if let Err(e) = server.await {
            error!(target: "helios::server", "Server error: {}", e);
        }
    });

    Ok(runtime)
}
