#!/bin/bash

# S3 hyperbeam NIF Server Runner

# S3 Configuration
export ENDPOINT="https://s3.load.rs"
export AWS_REGION="eu-west-2"
export AWS_ACCESS_KEY_ID="load_acc_D8Oa94kvS3bFECFUW9lhCGvNYMyZyLD4"
export AWS_SECRET_ACCESS_KEY=""
export PORT="8000"
export FORCE_PATH_STYLE="true"


echo "Starting S3 HB Server..."
echo "Endpoint: $ENDPOINT"
echo "Region: $AWS_REGION"
echo "Port: $PORT"
echo "Path Style: $FORCE_PATH_STYLE"
echo ""

cargo run --bin s3_nif