pub mod core;
pub mod tests;
use rustler::NifResult;
use std::collections::HashMap;
use crate::core::runtime::Runtime;

#[rustler::nif]
fn hello() -> NifResult<String> {
    Ok("Hello world!".to_string())
}

#[rustler::nif(schedule = "DirtyCpu")]
fn compute(
    num_qubits: usize,
    function_id: String,
    measurements: Vec<usize>,
) -> NifResult<HashMap<String, f64>> {
    let runtime = Runtime::new(num_qubits);
    
    match runtime.execute_serverless(function_id, measurements) {
        Ok(result) => Ok(result),
        Err(_) => Err(rustler::Error::Term(Box::new("execution failed"))),
    }
}

rustler::init!("quantum_runtime", [hello, compute]);