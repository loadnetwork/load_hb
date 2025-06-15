pub mod core;

use crate::core::light_client;
use rustler::{Env, Error, NifResult, ResourceArc, Term};
use std::net::SocketAddr;
use std::path::PathBuf;
use std::sync::Mutex;
use tokio::runtime::Runtime;
use tokio::sync::oneshot;

mod atoms {
    rustler::atoms! {
        ok,
        error,
        server_already_running,
        server_not_running,
        invalid_address,
    }
}

// Resource type to hold the server state
struct ServerResource {
    runtime: Mutex<Option<Runtime>>,
    shutdown_tx: Mutex<Option<oneshot::Sender<()>>>,
    address: Mutex<Option<SocketAddr>>,
}

#[rustler::nif(schedule = "DirtyCpu")]
fn start_helios(
    address: &str,
    execution_rpc: &str,
    consensus_rpc: &str,
    data_dir: &str,
) -> NifResult<ResourceArc<ServerResource>> {
    let socket_addr = address
        .parse::<SocketAddr>()
        .map_err(|_| Error::Atom("invalid_address"))?;

    let (shutdown_tx, shutdown_rx) = oneshot::channel();

    let data_dir_path = PathBuf::from(data_dir);

    let runtime = light_client::start_server(
        socket_addr,
        execution_rpc.to_string(),
        consensus_rpc.to_string(),
        data_dir_path,
        shutdown_rx,
    )
    .map_err(|err| Error::Term(Box::new(err.to_string())))?;

    // Create and return the resource
    let resource = ResourceArc::new(ServerResource {
        runtime: Mutex::new(Some(runtime)),
        shutdown_tx: Mutex::new(Some(shutdown_tx)),
        address: Mutex::new(Some(socket_addr)),
    });

    Ok(resource)
}

#[rustler::nif]
fn stop_server(resource: ResourceArc<ServerResource>) -> NifResult<()> {
    if let Some(tx) = resource.shutdown_tx.lock().unwrap().take() {
        let _ = tx.send(());
    } else {
        return Err(Error::Atom("server_not_running"));
    }

    if let Some(rt) = resource.runtime.lock().unwrap().take() {
        rt.shutdown_background();
    }

    *resource.address.lock().unwrap() = None;

    Ok(())
}

#[rustler::nif]
fn server_address(resource: ResourceArc<ServerResource>) -> NifResult<String> {
    if let Some(addr) = *resource.address.lock().unwrap() {
        Ok(addr.to_string())
    } else {
        Err(Error::Atom("server_not_running"))
    }
}

fn load(env: Env, _: Term) -> bool {
    let _ = rustler::resource!(ServerResource, env);
    true
}

rustler::init!(
    "helios_nif",
    [start_helios, stop_server, server_address],
    load = load
);
