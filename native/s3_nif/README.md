## About
s3 hyperbeam device

> MVP - WIP

## Setup

### 1- add s3_device.config

in the root level of the hyperbeam codebase, `touch s3_device.config` and add the creds to connect to your S3 cluster

#### connecting to external s3 cluster (`./build.sh`)

```config
{endpoint, <<"https://s3.load.rs">>}.
{access_key_id, <<"load_acc_XLrIyYcF6vdwr9tiug2wrLRSuSPmtucZ">>}.
{secret_access_key, <<>>}.
{region, <<"eu-west-2">>}.
```

#### connecting to local minio s3 cluster (`./s3_device.sh`)

```config
{endpoint, <<"http://localhost:9001">>}.
{access_key_id, <<"darwin">>}.
{secret_access_key, <<"darwin123">>}.
{region, <<"eu-west-2">>}.
```

### build and run the hyperbeam node 

```bash
./s3_device.sh # build the s3_nif device & run local minio cluster

# if you want to connect to external s3 cluster, run ./build.sh instead

rebar3 compile

erl -pa _build/default/lib/*/ebin 

1> application:ensure_all_started(hb).
```

### configurting the local minio cluster
if you choose the local minio cluster route, you can configure (set) your access key id and secret access key [here](../../minio-cluster/docker-compose.yml):

- `MINO_ROOT_USER` : access key id
- `MINIO_ROOT_PASSWORD` : secret access key

## Supported methods

| Supported  | 
| :-------------: |
| `create_bucket`| 
|`head_bucket`|
| `put_object`| 
| `get_object`|
|`delete_object`|
|`delete_objects`|
|`head_object`|
|`list_objects`|


## Use the ~s3@1.0 device

After running the hyperbeam node with the `~s3@1.0` device, you can use the `node_endpoint/~s3@1.0` url as a S3 compatible API endpoint.

### 1- create s3 client

```js
import { S3Client } from "@aws-sdk/client-s3";

const accessKeyId = "load_acc_XLrIyYcF6vdwr9tiug2wrLRSuSPmtucZ";
const secretAccessKey = "";

const s3Client = new S3Client({
  region: "eu-west-2",
  endpoint: "http://localhost:8734/~s3@1.0",
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
        console.log("Bucket created:", result.Location || bucketName);
    } catch (error) {
        console.error("Error creating bucket:", error);
    }
}
```

## Cache layer
The NIF implements an LRU cache with size-based eviction (in-memory). The following cache endpoints are available under the hyperbeam http api (intentionally not compatible with the S3 API spec):

### 1- get cached object

```bash
curl "http://localhost:8734/~s3@1.0/cache/BUCKET_NAME/OBJECT_KEY"
```

> cache vs S3 API `GetObjectCommand` : `curl "http://localhost:8734/~s3@1.0/BUCKET_NAME/OBJECT_KEY"`

## License
This repository is licensed under the [MIT License](./LICENSE)