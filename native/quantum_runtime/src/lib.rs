use roqoqo_quest::Backend;
pub mod core;
pub mod tests;

fn main() {
    let backend = Backend::new(2, None);
    println!("{:?}", backend);
    println!("roqoqo-quest installed successfully!");
}
