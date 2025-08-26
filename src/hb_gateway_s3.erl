-module(hb_gateway_s3).
-export([read/2, data/2]).
-include("include/hb.hrl").

%% @doc Read ANS-104 DataItem from S3's `s3_bucket` using dev_s3 device
read(ID, Opts) ->
    Bucket = hb_opts:get(s3_bucket, <<"offchain-dataitems">>, Opts),
    Key = <<"dataitems/", ID/binary, ".ans104">>,    
    
    io:format("S3 GATEWAY DEBUG: read() called for ID=~p, Bucket=~p, Key=~p~n", [ID, Bucket, Key]),

    case dev_s3:get_object_handler(Bucket, Key, #{}, Opts#{internal => true}) of
        {ok, #{<<"body">> := ANS104Data, <<"status">> := 200}} ->
            io:format("S3 GATEWAY DEBUG: S3 retrieval successful, data type=~p, size=~p~n", [type(ANS104Data), byte_size_safe(ANS104Data)]),
            Result = parse_stored_ans104(ANS104Data, Opts),
            io:format("S3 GATEWAY DEBUG: parse_stored_ans104 result=~p~n", [element(1, Result)]),
            Result;
        {error, #{<<"status">> := 404}} ->
            io:format("S3 GATEWAY DEBUG: S3 returned 404~n"),
            not_found;
        {error, Reason} ->
            io:format("S3 GATEWAY DEBUG: S3 error=~p~n", [Reason]),
            {error, Reason}
    end.

%% Helper function to safely get byte size
byte_size_safe(Data) when is_binary(Data) -> byte_size(Data);
byte_size_safe(Data) when is_list(Data) -> length(Data);
byte_size_safe(_) -> unknown.

%% Helper function to get type
type(Data) when is_binary(Data) -> binary;
type(Data) when is_list(Data) -> list;
type(_) -> unknown.

%% @doc Get raw data from ANS-104 DataItem
data(ID, Opts) ->
    case read(ID, Opts) of
        {ok, #{<<"data">> := Data}} -> {ok, Data};
        {ok, Message} -> 
            {ok, maps:get(<<"body">>, Message, <<>>)};
        {error, Reason} -> {error, Reason}
    end.

%% @doc Parse S3-stored ANS-104 format into HyperBEAM message format
parse_stored_ans104(RawData, Opts) ->
    io:format("S3 GATEWAY DEBUG: parse_stored_ans104 called, data type=~p, size=~p~n", [type(RawData), byte_size_safe(RawData)]),
    
    % Convert to binary if it's a list
    Binary = case RawData of
        Data when is_binary(Data) -> 
            io:format("S3 GATEWAY DEBUG: Data is already binary~n"),
            Data;
        Data when is_list(Data) -> 
            io:format("S3 GATEWAY DEBUG: Converting list to binary~n"),
            list_to_binary(Data);
        _ -> 
            io:format("S3 GATEWAY DEBUG: Unknown data type, trying to convert~n"),
            iolist_to_binary(RawData)
    end,
    
    io:format("S3 GATEWAY DEBUG: After conversion - binary size=~p~n", [byte_size(Binary)]),
    
    try 
        io:format("S3 GATEWAY DEBUG: Calling ar_bundles:deserialize~n"),
        % Deserialize as ANS-104 binary format
        case ar_bundles:deserialize(Binary) of
            TX when is_record(TX, tx) ->
                io:format("S3 GATEWAY DEBUG: ar_bundles:deserialize successful, TX record created~n"),
                % Convert TX record to HyperBEAM message format
                Message = tx_to_message(TX, Opts),
                io:format("S3 GATEWAY DEBUG: tx_to_message successful, message keys=~p~n", [maps:keys(Message)]),
                {ok, Message};
            Other ->
                io:format("S3 GATEWAY DEBUG: ar_bundles:deserialize returned unexpected result=~p~n", [Other]),
                {error, invalid_ans104_format}
        end
    catch
        Error:Reason:Stacktrace ->
            io:format("S3 GATEWAY DEBUG: Exception in parse_stored_ans104 - Error=~p, Reason=~p, Stacktrace=~p~n", [Error, Reason, Stacktrace]),
            {error, failed_to_parse_ans104}
    end.

%% @doc Convert TX record to HyperBEAM message format
tx_to_message(TX, _Opts) ->
    TagFields = tx_tags_to_message_fields(TX#tx.tags),
    BaseMessage = #{
        <<"data">> => TX#tx.data,
        <<"id">> => hb_util:encode(hb_util:id(TX, signed)),
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
