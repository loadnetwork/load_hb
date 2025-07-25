# Custom process deployment


## Create process dataitem 
```erlang
rr("src/ar_tx.erl").  % load tx record definition

Data = <<"hello load world">>.

Tags = [
    {<<"Authority">>, <<"fcoN_xJeisVsPXA-trzVAuIiqO3ydLQxM-L4XbrQKzY">>},
    {<<"On-Boot">>, <<"dvlU-5HfAPWcukvnRHwSzMSEsoCqkZAEQsykMQgQ-wE">>},
    {<<"Creator">>, <<"D75xZnMiLlnnj7u93JiNX7wm3ZfSew3TPdK1kRAbhqw">>},
    {<<"Asset-Type">>, <<"Example Atomic Asset Type">>},
    {<<"Content-Type">>, <<"text/plain">>},
    {<<"Implements">>, <<"ANS-110">>},
    {<<"Date-Created">>, <<"1752869180258">>},
    {<<"Bootloader-Name">>, <<"Example Name">>},
    {<<"Bootloader-Description">>, <<"Example Description">>},
    {<<"Bootloader-Topics">>, <<"[\"Topic 1\",\"Topic 2\",\"Topic 3\"]">>},
    {<<"Bootloader-Ticker">>, <<"ATOMIC">>},
    {<<"Bootloader-Denomination">>, <<"1">>},
    {<<"Bootloader-TotalSupply">>, <<"1">>},
    {<<"Bootloader-Transferable">>, <<"true">>},
    {<<"Bootloader-Creator">>, <<"D75xZnMiLlnnj7u93JiNX7wm3ZfSew3TPdK1kRAbhqw">>},
    {<<"Bootloader-Status">>, <<"Initial Status">>},
    {<<"SDK">>, <<"aoconnect">>},
    {<<"Data-Protocol">>, <<"ao">>},
    {<<"Variant">>, <<"ao.TN.1">>},
    {<<"Type">>, <<"Process">>},
    {<<"Module">>, <<"URgYpPQzvxxfYQtjrIQ116bl3YBfcImo3JEnNo8Hlrk">>},
    {<<"Scheduler">>, <<"_GQ33BkPtZrqxA84vM8Zk-N2aO0toNNu_C-l-rawrBA">>}
].

TX = #tx{data = Data, tags = Tags, format = ans104}.
SignedTX = ar_bundles:sign_item(TX, hb:wallet()).
ANS104Binary = ar_bundles:serialize(SignedTX).
DataItemID = hb_util:encode(hb_tx:id(SignedTX, signed)).
file:write_file("TheDataItemId.ans104", ANS104Binary).
% now save the data item on the cluster of ~s3@1.0
```

## upload it to ~s3@1.0

N.B to change `YOUR_ACCESS_KEY`

```bash
curl -X PUT "http://localhost:8734/~s3@1.0/offchain-dataitems/dataitems/TheDataItemId.ans104" \
  -H "Content-Type: application/octet-stream" \
  -H "Authorization: AWS4-HMAC-SHA256 Credential=YOUR_ACCESS_KEY_ID/20250119/us-east-1/s3/aws4_request, SignedHeaders=host;x-amz-date, Signature=dummy" \
  --data-binary TheDataItemId-E.ans104
  ```
Access the data from the hybrid gateway at https://localhost:8734/TheDataItemId

## Load the process

```bash
curl "http://localhost:8734/Mg6LKY1H4NbDdUc47_OT_d6DISmldk7HdO04wW9GoMM~process@1.0/now/serialize~json@1.0"

curl "http://localhost:8734/Mg6LKY1H4NbDdUc47_OT_d6DISmldk7HdO04wW9GoMM~process@1.0/now/Bootloader-Name"
```