use crate::core::runtime::Runtime;
use roqoqo::Circuit;
use roqoqo::operations;
use std::collections::HashMap;

impl Runtime {
    pub fn get_function_registry(&self) -> HashMap<String, Box<dyn Fn() -> Circuit>> {
        let mut registry: HashMap<String, Box<dyn Fn() -> Circuit>> = HashMap::new();
        let max_qubits = self.num_qubits;

        registry.insert(
            "superposition".to_string(),
            Box::new(move || {
                let mut circuit = Circuit::new();
                circuit += operations::Hadamard::new(0);
                circuit
            }),
        );

        registry.insert(
            "quantum_rng".to_string(),
            Box::new(move || {
                let bits = std::cmp::min(max_qubits, 8);
                let mut circuit = Circuit::new();
                for qubit in 0..bits {
                    circuit += operations::Hadamard::new(qubit);
                }
                circuit
            }),
        );

        registry
    }
}
