## About
s3 hyperbeam device

## Setup

### 1- add s3_device.config

in the root level of the hyperbeam codebase, `touch s3_device.config` and add the creds to connect to your S3 cluster

```config
{endpoint, <<"https://s3.load.rs">>}.
{access_key_id, <<"load_acc_XLrIyYcF6vdwr9tiug2wrLRSuSPmtucZ">>}.
{secret_access_key, <<>>}.
{region, <<"eu-west-2">>}.
```

### build and run the hyperbeam node 

```bash
./build.sh

rebar3 compile

erl -pa _build/default/lib/*/ebin 

1> application:ensure_all_started(hb).
```

## Use the ~s3@1.0 device

After running the hyperbeam node with the `~s3@1.0` device, you can use the `node_endpoint/~s3@1.0` url as a S3 compatible API endpoint.

### 1- create s3 client

```js
import { S3Client } from "@aws-sdk/client-s3";

const accessKeyId = "load_acc_XLrIyYcF6vdwr9tiug2wrLRSuSPmtucZ";
const secretAccessKey = "";

const s3Client = new S3Client({
  region: "eu-west-2",
  endpoint: "http://localhost:10001/~s3@1.0",
  credentials: {
    accessKeyId,
    secretAccessKey,
  },
  forcePathStyle: true,
});
```

### 2- create bucket

```js
async function createBucket(bucketName) {
    try {
        const command = new CreateBucketCommand({ Bucket: bucketName });
        const result = await s3Client.send(command);
        console.log("✅ Bucket created:", result.Location || bucketName);
    } catch (error) {
        console.error("❌ Error creating bucket:", error);
    }
}
```

## License
This repository is licensed under the [MIT License](./LICENSE)