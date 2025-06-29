use crate::core::runtime::Runtime;
use crate::tests::functions::{quantum_rng, superposition};
use roqoqo::Circuit;
use std::collections::HashMap;

impl Runtime {
    pub fn get_function_registry(&self) -> HashMap<String, Box<dyn Fn() -> Circuit>> {
        let mut registry: HashMap<String, Box<dyn Fn() -> Circuit>> = HashMap::new();
        let max_qubits = self.num_qubits;

        registry.insert(
            "superposition".to_string(),
            Box::new(move || superposition(1)),
        );

        registry.insert(
            "quantum_rng".to_string(),
            Box::new(move || quantum_rng(max_qubits)),
        );

        registry
    }
}
