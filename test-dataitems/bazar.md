# Bazar NFT Creation

## Create dataitem

```erlang
rr("src/ar_tx.erl").  % load tx record definition

% Read image data
{ok, Data} = file:read_file("test-data/computer.jpg").

% Load the arweave wallet
Wallet = ar_wallet:load_keyfile("your_wallet_path.json").

Tags = [
    {<<"Access-Fee">>, <<"None">>},
    {<<"Asset-Type">>, <<"image/jpg">>}, % remember to update to the proper MIME incase you change the file
    {<<"Authority">>, <<"fcoN_xJeisVsPXA-trzVAuIiqO3ydLQxM-L4XbrQKzY">>},
    {<<"Bootloader-CollectionId">>, <<"3gIE9kDWmMp96mctpTTkp4RCGYSGfacGOV6rtAMuC5s">>},
    {<<"Bootloader-Creator">>, <<"CbHyxgHSyDaYYVwH4gXe4YLoGz-8fyYit2YIkkqhrG8">>},
    {<<"Bootloader-Denomination">>, <<"1">>},
    {<<"Bootloader-Description">>, <<"[^^]">>},
    {<<"Bootloader-Name">>, <<"COMPUTERRR WINDOWSSSSS">>},
    {<<"Bootloader-Ticker">>, <<"ATOMIC">>},
    {<<"Bootloader-Topics">>, <<"[\"ALBUM\"]">>},
    {<<"Bootloader-TotalSupply">>, <<"100">>},
    {<<"Bootloader-Transferable">>, <<"true">>},
    {<<"Commercial-Use">>, <<"Allowed-With-One-Time-Fee-0.01">>},
    {<<"Creator">>, <<"CbHyxgHSyDaYYVwH4gXe4YLoGz-8fyYit2YIkkqhrG8">>},
    {<<"Currency">>, <<"xU9zFkq3X2ZQ6olwNVvr1vUWIjc3kXTWr7xKQD6dh10">>},
    {<<"Data-Model-Training">>, <<"Disallowed">>},
    {<<"Date-Created">>, <<"1753740544182">>},
    {<<"Derivations">>, <<"Allowed-With-One-Time-Fee-0.01">>},
    {<<"Implements">>, <<"ANS-110">>},
    {<<"License">>, <<"dE0rmDfl9_OWjkDznNEXHaSO_JohJkRolvMzaCroUdw">>},
    {<<"On-Boot">>, <<"dvlU-5HfAPWcukvnRHwSzMSEsoCqkZAEQsykMQgQ-wE">>},
    {<<"Payment-Address">>, <<"vZY2XY1RD9HIfWi8ift-1_DnHLDadZMWrufSh-_rKF0">>}, % change to your wallet address
    {<<"Payment-Mode">>, <<"Single">>},
    {<<"Content-Type">>, <<"image/jpg">>}, % remember to update to the proper MIME incase you change the file
    {<<"Data-Protocol">>, <<"ao">>},
    {<<"Scheduler">>, <<"_GQ33BkPtZrqxA84vM8Zk-N2aO0toNNu_C-l-rawrBA">>},
    {<<"SDK">>, <<"s3@1.0">>},
    {<<"Variant">>, <<"ao.TN.1">>},
    {<<"Type">>, <<"Process">>},
    {<<"Module">>, <<"URgYpPQzvxxfYQtjrIQ116bl3YBfcImo3JEnNo8Hlrk">>}
].

TX = #tx{data = Data, tags = Tags, format = ans104}.
SignedTX = ar_bundles:sign_item(TX, Wallet).
ANS104Binary = ar_bundles:serialize(SignedTX).
DataItemID = hb_util:encode(hb_tx:id(SignedTX, signed)).
file:write_file("test-dataitems/DataItem.ans104", ANS104Binary).
io:format("DataItem ID: ~s~n", [DataItemID]).
```

## Upload it to ~s3@1.0

N.B to change `YOUR_ACCESS_KEY_ID`

```bash
curl -X PUT "http://localhost:8734/~s3@1.0/offchain-dataitems/dataitems/DataItem.ans104" \
  -H "Content-Type: application/octet-stream" \
  -H "Authorization: AWS4-HMAC-SHA256 Credential=YOUR_ACCESS_KEY_ID/20250119/us-east-1/s3/aws4_request, SignedHeaders=host;x-amz-date, Signature=dummy" \
  --data-binary @DataItem.ans104
```

Access the data from the hybrid gateway at https://localhost:8734/[DataItemID]

## Make AO aware of the process

Once you want to post the offchain dataitem (process) to AO (onchain), you need to "register" the process dataitem in the ao network. check [aojs example](./aojs/) on how to do so (remember to update the `S3_DATAITEM_ID`)

## Action : Init

After registering the ao process in the MU, you need to Init the process. For the sake of simplicity, use the ao.link process `write` tab

`https://www.ao.link/#/entity/DataItemId?tab=write`

![](https://gateway.load.rs/bundle/0x533c0595649265e56b87d6b857f37bf7c30665fdc9f00e22e2877b817f64e3bb/0)

## Bazar is aware of the process

Access: `https://bazar.arweave.net/#/asset/DataItemId` (example: https://bazar.arweave.net/#/asset/yXuCzcOjglttEKRqW7dDYeNy2s_S9eSSFkwzFYRVATg)