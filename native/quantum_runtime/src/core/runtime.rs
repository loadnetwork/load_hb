use anyhow::Error;
use roqoqo::Circuit;
use roqoqo::backends::EvaluatingBackend;
use roqoqo::operations;
use roqoqo_quest::Backend;
use std::collections::HashMap;

pub struct Runtime {
    pub backend: Backend,
    pub num_qubits: usize,
}

impl Runtime {
    pub fn new(num_qubits: usize) -> Self {
        // let device = AqtDevice::new(num_qubits);
        Self {
            backend: Backend::new(num_qubits, None), // todo: add random seed, but it's not that serious as it's simulation
            // backend: AQTBackend::new(device, Some("your_api_token".to_string())).unwrap(), // once AQT is supported
            num_qubits,
        }
    }

    pub fn execute<F>(
        &self,
        quantum_fn: F,
        measurements: Vec<usize>,
    ) -> Result<HashMap<String, f64>, anyhow::Error>
    where
        F: FnOnce() -> Circuit,
    {
        let mut circuit = quantum_fn();

        // classical register
        circuit += operations::DefinitionBit::new("ro".to_string(), measurements.len(), true);

        // add measurements
        for (index, &qubit) in measurements.iter().enumerate() {
            circuit += operations::MeasureQubit::new(qubit, "ro".to_string(), index);
        }

        circuit += operations::PragmaSetNumberOfMeasurements::new(1, "ro".to_string());

        let (bit_result, _float_result, _complex_result) = self.backend.run_circuit(&circuit)?;

        // convert bit results to HashMap<String, f64>
        let mut result = HashMap::new();
        if let Some(measurements_vec) = bit_result.get("ro") {
            for (shot_idx, shot_results) in measurements_vec.iter().enumerate() {
                for (bit_idx, &bit_value) in shot_results.iter().enumerate() {
                    let key = format!("qubit_{}", measurements[bit_idx]);
                    result.insert(key, if bit_value { 1.0 } else { 0.0 });
                }
            }
        }

        Ok(result)
    }

    // pub fn execute_serverless(&self, function_id: String, measurements: Vec<usize>) -> Result<HashMap<String, f64>, Error> {
    //     let _ = parse_function(function_id)?;
    //     let quantum_fn = std::fs::read_to_string();
    //     self.execute(quantum_fn, measurements)
        
    // }
}
