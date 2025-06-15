## About
The `~helios@1.0` device is an EVM/Ethereum consensus light client built into the HyperBEAM devices stack. With helios, node operators can trustlessly connect to EVM RPCs with a very lightweight, multichain and secure setup, and no historical syncing overhead. With this device, every hyperbeam node can turn into a verifiable EVM RPC endpoint.

### What is Helios?

> Helios is a trustless, efficient, and portable multichain light client written in Rust.

> Helios converts an untrusted centralized RPC endpoint into a safe unmanipulable local RPC for its users. It syncs in seconds, requires no storage, and is lightweight enough to run on mobile devices.

> Helios has a small binary size and compiles into WebAssembly. This makes it a perfect target to embed directly inside wallets and dapps.

Check out the official repository [here](https://github.com/a16z/helios)

## ~helios@1.0 device

The `~helios@1.0` as per its current implementation, initiates the helios client (and JSON-RPC server) at the start of the hyperbeam node run. The JSON-RPC server is spawned as a separate process running in parallel behind the `8545` port (standard consensus rpc port).

The device supports all of the methods supported by helios. Check the full list [here](https://github.com/a16z/helios/blob/master/rpc.md)

### Endpoint
As this device is supported on the [hb.load.rs](https://hb.load.rs) hyperbeam node, it's explicitly assigned the `eth.rpc.rs` endpoint for the Ethereum mainnet network.

### Example

#### local 
```bash
curl -X POST -H "Content-Type: application/json" --data '{"jsonrpc":"2.0","method":"eth_blockNumber","params":[],"id":1}' http://127.0.0.1:8545
```

#### using rpc.rs 
```bash
curl -X POST -H "Content-Type: application/json" --data '{"jsonrpc":"2.0","method":"eth_blockNumber","params":[],"id":1}' https://eth.rpc.rs
```
## License
This repository is licensed under the [MIT License](./LICENSE)