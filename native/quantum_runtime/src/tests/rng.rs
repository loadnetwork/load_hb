
#[cfg(test)]
mod tests {
    use super::*;
    use crate::core::runtime::Runtime;
    use crate::tests::functions::quantum_rng;
    use std::collections::HashMap;

    #[test]
    fn test_quantum_rng() {
        let runtime = Runtime::new(8);
        println!("Quantum Random Number Generator:");

        // 4-bit random number (0-15)
        let result = runtime
            .execute(|| quantum_rng(4), (0..4).collect())
            .unwrap();
        let random_number = bits_to_number(&result, 4);
        println!("   Random 4-bit: {} (0-15)", random_number);

        // 8-bit random number (0-255)
        let result = runtime
            .execute(|| quantum_rng(8), (0..8).collect())
            .unwrap();
        let random_number = bits_to_number(&result, 8);
        println!("   Random 8-bit: {} (0-255)", random_number);

        fn bits_to_number(result: &HashMap<String, f64>, num_bits: usize) -> u32 {
            let mut number = 0u32;
            for i in 0..num_bits {
                let bit_key = format!("qubit_{}", i);
                if result.get(&bit_key).unwrap_or(&0.0) == &1.0 {
                    number |= 1 << i;
                }
            }
            number
        }
    }
}
