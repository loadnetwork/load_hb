-module(hb_gateway_s3).
-export([read/2, data/2]).
-include("include/hb.hrl").

%% @doc Read ANS-104 DataItem from S3's `s3_bucket` using dev_s3 device
read(ID, Opts) ->
    Bucket = hb_opts:get(s3_bucket, <<"offchain-dataitems">>, Opts),
    Key = <<"dataitems/", ID/binary, ".ans104">>,
    
    % Create message for dev_s3 device to retrieve the dataitem object
    S3Msg = #{
        <<"method">> => <<"GET">>,
        <<"body">> => <<>>,
        <<"headers">> => #{},
        <<"query">> => <<>>
    },
    
    case dev_s3:handle_s3_request(<<"GET">>, <<"/~s3@1.0/", Bucket/binary, "/", Key/binary>>, S3Msg, Opts) of
        {ok, #{<<"body">> := ANS104Data, <<"status">> := 200}} ->
            parse_stored_ans104(ANS104Data, Opts);
        {error, #{<<"status">> := 404}} ->
            {error, not_found};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Get raw data from ANS-104 DataItem
data(ID, Opts) ->
    case read(ID, Opts) of
        {ok, #{<<"data">> := Data}} -> {ok, Data};
        {ok, Message} -> 
            {ok, maps:get(<<"body">>, Message, <<>>)};
        {error, Reason} -> {error, Reason}
    end.

%% @doc Parse S3-stored ANS-104 format into HyperBEAM message format
parse_stored_ans104(Binary, Opts) ->
    try 
        % Deserialize as ANS-104 binary format
        case ar_bundles:deserialize(Binary) of
            TX when is_record(TX, tx) ->
                % Convert TX record to HyperBEAM message format
                {ok, tx_to_message(TX, Opts)};
            _ ->
                {error, invalid_ans104_format}
        end
    catch
        _:_ ->
            {error, failed_to_parse_ans104}
    end.

%% @doc Convert TX record to HyperBEAM message format
tx_to_message(TX, _Opts) ->
    TagFields = tx_tags_to_message_fields(TX#tx.tags),
    BaseMessage = #{
        <<"data">> => TX#tx.data,
        <<"id">> => hb_util:encode(hb_tx:id(TX, signed)),
        <<"tags">> => TX#tx.tags,
        <<"owner">> => hb_util:encode(TX#tx.owner),
        <<"signature">> => hb_util:encode(TX#tx.signature)
    },
    maps:merge(BaseMessage, TagFields).

%% @doc Convert TX tags to message fields
tx_tags_to_message_fields(Tags) ->
    lists:foldl(
        fun({Name, Value}, Acc) ->
            Acc#{Name => Value}
        end,
        #{},
        Tags
    ).
