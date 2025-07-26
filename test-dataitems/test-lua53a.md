# Deploy and interact with offchain lua@5.3a process



## Create process dataitem

Create your process from the [lua source code](../test/lua_device_token.lua) following the same steps in [test-process.md](../test-dataitems/test-process.md) - or simply store the offchain created data item to your ~s3@1.0 cluster:

```bash
curl -X PUT "http://localhost:8734/~s3@1.0/offchain-dataitems/dataitems/EijdV3U1GDRlcrDn4vNpozZcF4ld62gwPQQJQ0Q6Cz0.ans104" \
  -H "Content-Type: application/octet-stream" \
  -H "Authorization: AWS4-HMAC-SHA256 Credential=YOUR_ACCESS_KEY_ID/20250119/us-east-1/s3/aws4_request, SignedHeaders=host;x-amz-date, Signature=dummy" \
  --data-binary @EijdV3U1GDRlcrDn4vNpozZcF4ld62gwPQQJQ0Q6Cz0.ans104
```

## Interact with the process

```erlang
TokenLuaID = <<"EijdV3U1GDRlcrDn4vNpozZcF4ld62gwPQQJQ0Q6Cz0">>.

TokenDevice = #{
    <<"device">> => <<"lua@5.3a">>,
    <<"module">> => TokenLuaID,
    <<"store-gateway">> => #{<<"store-module">> => hb_store_gateway}
}.

{ok, InitializedToken} = hb_ao:resolve(TokenDevice, <<"init">>, #{}).
io:format("Token device initialized successfully!~n").

% Test Token Info function
{ok, InfoResult} = hb_ao:resolve(InitializedToken, <<"Info">>, #{}).
io:format("Token Info: ~p~n", [InfoResult]).

% Test Balance function with target
BalanceMessage = #{<<"path">> => <<"Balance">>, <<"target">> => <<"test-address-123">>}.
{ok, BalanceResult} = hb_ao:resolve(InitializedToken, BalanceMessage, #{}).
io:format("Balance result: ~p~n", [BalanceResult]).
```