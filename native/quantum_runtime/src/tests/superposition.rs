#[cfg(test)]
mod tests {
    use crate::core::runtime::Runtime;
    use crate::tests::functions::superposition;

    
    #[test]
    fn test_superposition() {
        let vm = Runtime::new(2);
        
        // Test superposition
        let result = vm.execute(|| superposition(1), vec![1]).unwrap();
        println!("Test result: {:?}", result);
    }
}