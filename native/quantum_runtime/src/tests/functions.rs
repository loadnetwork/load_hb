use roqoqo::Circuit;
use roqoqo::operations;

// ===== QUANTUM SERVERLESS FUNCTIONS =====

/// Quantum function: Create superposition
/// load function id: 0x32a2fd8f46ac4e7abb0e02a03cb47c2f4d3fbd45dd93d1f333b535c515a2ad18
pub fn superposition(qubit: usize) -> Circuit {
    let mut circuit = Circuit::new();
    circuit += roqoqo::operations::Hadamard::new(qubit);
    circuit
}

/// Quantum function: Quantum NOT gate
pub fn quantum_not(qubit: usize) -> Circuit {
    let mut circuit = Circuit::new();
    circuit += roqoqo::operations::PauliX::new(qubit);
    circuit
}

/// Quantum function: Create Bell state (entanglement)
pub fn bell_state(control: usize, target: usize) -> Circuit {
    let mut circuit = Circuit::new();
    circuit += operations::Hadamard::new(control);
    circuit += operations::CNOT::new(control, target);
    circuit
}

/// Quantum function: Quantum teleportation protocol
pub fn quantum_teleportation(alice: usize, bob: usize, msg: usize) -> Circuit {
    let mut circuit = Circuit::new();

    // step 1: create entangled pair between Alice and Bob
    circuit += operations::Hadamard::new(alice);
    circuit += operations::CNOT::new(alice, bob);

    // step 2: alice applies operations to her qubit and message
    circuit += operations::CNOT::new(msg, alice);
    circuit += operations::Hadamard::new(msg);

    // step 3: Bob applies corrections based on Alice's measurements
    circuit += operations::CNOT::new(alice, bob);
    circuit += operations::ControlledPauliZ::new(msg, bob);

    circuit
}

/// Quantum function: Generate random number (0 to 2^bits - 1)
/// as it's simulation, therefore it's pseudo-random
pub fn quantum_rng(bits: usize) -> Circuit {
    let mut circuit = Circuit::new();

    // Put each qubit in superposition for true quantum randomness
    for qubit in 0..bits {
        circuit += operations::Hadamard::new(qubit);
    }

    circuit
}
