#[cfg(test)]
mod tests {
    use std::result;

    use crate::core::runtime::Runtime;
    use crate::tests::functions::superposition;

    #[test]
    fn test_superposition() {
        let rt = Runtime::new(2);

        // Test superposition
        let result = rt.execute(|| superposition(1), vec![1]).unwrap();
        println!("Test result: {:?}", result);
    }

    #[test]
    fn test_superposition_serverless() {
        let rt = Runtime::new(2);
        let result = rt
            .execute_serverless("superposition".to_string(), vec![1])
            .unwrap();
        println!("serverless superposition test result: {:?}", result);
    }
}
