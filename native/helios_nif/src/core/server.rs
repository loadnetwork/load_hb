use anyhow::Error;
use helios::ethereum::{EthereumClientBuilder, config::networks::Network};
use hyper::{
    Body, Response, Server,
    service::{make_service_fn, service_fn},
};
use serde_json::Value;
use std::convert::Infallible;
use std::net::SocketAddr;
use std::path::PathBuf;
use std::sync::{Arc, Mutex};
use tokio::runtime::Runtime;
use tokio::sync::oneshot;
use tracing::{error, info};
use tracing_subscriber::FmtSubscriber;
use tracing_subscriber::filter::{EnvFilter, LevelFilter};

use crate::core::types::{JsonRpcError, JsonRpcErrorResponse, ServerState};
use crate::core::light_client::handle_request;


pub fn create_error_response(
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
