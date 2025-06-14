use anyhow::Error;
use serde_json::Value;
use ureq;

pub fn query_permaweb() -> Result<String, Error> {
    let url = "https://arweave-search.goldsky.com/graphql/graphql";

    let query = r#"
        query {
          transactions(
            first: 1
            sort: HEIGHT_DESC
            tags: [
              {
                name: "Content-Type"
                values: [
                  "image/png"
                  "image/jpeg"
                  "image/webp"
                  "image/gif"
                  "image/svg+xml"
                  "image/avif"
                ]
                op: EQ
                match: EXACT
              }
            ]
          ) {
            edges {
              node {
                id
                tags {
                  name
                  value
                }
                block {
                  id
                  height
                  timestamp
                  previous
                }
              }
            }
          }
        }
    "#;

    let body = serde_json::json!({ "query": query });

    let response = ureq::post(url)
        .header("Content-Type", "application/json")
        .header("Accept", "application/json")
        .header("DNT", "1")
        .header("Origin", "https://arweave-search.goldsky.com")
        .send_json(body)?;

    let json: Value = response.into_body().read_json()?;

    let tx_id = &json["data"]["transactions"]["edges"][0]["node"]["id"];
    let tx_id = tx_id.as_str().unwrap_or("No ID found").to_string();

    Ok(tx_id)
}
