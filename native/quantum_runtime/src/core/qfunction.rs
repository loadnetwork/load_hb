use ureq;
use anyhow::Error;
use std::fs;

pub fn download_function(function_id: &str) -> Result<String, Error> {
    let url = format!("https://load0.network/download/{}", function_id);
    let res = ureq::get(url).call()?.into_body().read_to_string()?;
    Ok(res)
}

pub fn parse_function(function_id: String) -> Result<(), Error> {
    let function_src = download_function(&function_id)?;
    let serverless_fn = format!("use roqoqo::Circuit; use roqoqo::operations; {}", &function_src);

    let dir_path = "q_functions";
    fs::create_dir_all(dir_path)?;
    
    let fn_path = format!("{}/{}.rs", dir_path, function_id);
    fs::write(&fn_path, serverless_fn)?;

    println!("Successfully saved quantum function to: {}", fn_path);
    Ok(())
}