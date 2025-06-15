use serde_json::Value;
use serde::{Serialize, Deserialize};
use tokio::sync::oneshot;
use std::sync::Arc;

#[derive(Deserialize, Debug)]
pub struct JsonRpcRequest {
    pub jsonrpc: String,
    pub method: String,
    pub params: Vec<Value>,
    pub id: Value,
}

#[derive(Serialize)]
pub struct JsonRpcResponse {
    pub jsonrpc: String,
    pub result: Value,
    pub id: Value,
}

#[derive(Serialize)]
pub struct JsonRpcErrorResponse {
    pub jsonrpc: String,
    pub error: JsonRpcError,
    pub id: Value,
}

#[derive(Serialize)]
pub struct JsonRpcError {
    pub code: i32,
    pub message: String,
}

pub struct ServerState {
    pub client: Arc<helios::ethereum::EthereumClient>,
    pub shutdown_signal: Option<oneshot::Receiver<()>>,
}