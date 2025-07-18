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
        case hb_json:decode(Binary) of
            #{<<"data">> := _, <<"tags">> := Tags} = Message ->
                % Convert tags to direct message fields
                % the same way fn subindex_to_tags() handles it in hb_gateway_client.erl:
                TagFields = tags_to_message_fields(Tags),
                {ok, maps:merge(Message, TagFields)};
            #{<<"data">> := Data} = Message ->
                Tags = maps:get(<<"tags">>, Message, []),
                TagFields = tags_to_message_fields(Tags),
                {ok, maps:merge(Message#{<<"tags">> => Tags}, TagFields)};
            DecodedData when is_map(DecodedData) ->
                Tags = maps:get(<<"tags">>, DecodedData, []),
                TagFields = tags_to_message_fields(Tags),
                BaseMessage = #{
                    <<"data">> => maps:get(<<"data">>, DecodedData, DecodedData),
                    <<"id">> => maps:get(<<"id">>, DecodedData, hb_util:id(Binary)),
                    <<"tags">> => Tags,
                    <<"owner">> => maps:get(<<"owner">>, DecodedData, <<>>),
                    <<"signature">> => maps:get(<<"signature">>, DecodedData, <<>>)
                },
                {ok, maps:merge(BaseMessage, TagFields)};
            DecodedData ->
                {ok, #{
                    <<"data">> => DecodedData,
                    <<"id">> => hb_util:id(Binary),
                    <<"tags">> => []
                }}
        end
    catch
        _:_ ->
            {ok, #{
                <<"data">> => Binary,
                <<"id">> => hb_util:id(Binary),
                <<"tags">> => []
            }}
    end.


%% @doc Convert JSON tags array to HyperBEAM message fields
tags_to_message_fields(Tags) when is_list(Tags) ->
    lists:foldl(
        fun(#{<<"name">> := Name, <<"value">> := Value}, Acc) ->
            Acc#{Name => Value};
           (_, Acc) -> Acc
        end,
        #{},
        Tags
    );
tags_to_message_fields(_) -> #{}.
