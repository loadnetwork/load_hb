%%% @doc S3 GraphQL query support for dataitems stored in Load S3 bucket.
%%% Provides fallback functionality when local cache queries return no results
%%% Treating Load S3 offchain dataitems with lowest precedency, ensuring smooth
%%% priority querying for hybrid arweave-loads3 dataitems
-module(dev_query_s3).
-export([query_transactions/2, match_tags/2, list_dataitems/1]).
-include("include/hb.hrl").

%% @doc Query Load S3 for transactions matching the given GQL arguments
query_transactions(Args, Opts) ->
    ?event({s3_query_transactions, Args}),
    
    % For simplicity, list all dataitems in load s3 bucket then filter 
    % them against the GQL query arguments
    case list_dataitems(Opts) of
        {ok, DataitemIDs} ->
            ?event({s3_found_dataitems, length(DataitemIDs)}),
            % Filter the s3 object against GQL arguments
            FilteredIDs = filter_dataitems(DataitemIDs, Args, Opts),
            ?event({s3_filtered_dataitems, length(FilteredIDs)}),
            % Load the actual messages (compatible fmt) for the filtered dataitem IDs
            load_s3_messages(FilteredIDs, Opts);
        {error, Reason} ->
            ?event({s3_query_error, Reason}),
            {ok, []}
    end.


%% @doc Match offchain dataitems by tag filters
match_tags(Tags, Opts) ->
    ?event({s3_match_tags, Tags}),
    
    % Convert GQL tags format to s3 internal format
    TagMap = dev_query_graphql:keys_to_template(Tags),
    ?event({s3_tag_map, TagMap}),
    
    case list_dataitems(Opts) of
        {ok, DataitemIDs} ->
            % Filter dataitems
            MatchingIDs = filter_by_tags(DataitemIDs, TagMap, Opts),
            ?event({s3_tag_matches, length(MatchingIDs)}),
            load_s3_messages(MatchingIDs, Opts);
        {error, Reason} ->
            ?event({s3_tag_match_error, Reason}),
            {ok, []}
    end.

%% @doc List all available dataitems (ans104 objects) in the Load S3 bucket
list_dataitems(Opts) ->
    Bucket = hb_opts:get(s3_bucket, <<"offchain-dataitems">>, Opts),
    Prefix = <<"dataitems/">>,
    
    ?event({s3_list_dataitems, {bucket, Bucket}, {prefix, Prefix}}),
    
    case dev_s3:list_objects_handler(Bucket, #{<<"prefix">> => Prefix}, #{}, Opts#{internal => true}) of
        {ok, #{<<"body">> := Objects, <<"status">> := 200}} ->
            ?event({s3_list_success, {objects, length(Objects)}}),
            DataitemIDs = extract_dataitem_ids(Objects),
            {ok, DataitemIDs};
        {error, Reason} ->
            ?event({s3_list_error, Reason}),
            {error, Reason}
    end.

%% @doc Extract dataitems IDs from S3 object list
extract_dataitem_ids(Objects) ->
    lists:filtermap(
        fun(Object) ->
            case hb_maps:get(<<"key">>, Object, not_found, #{}) of
                <<"dataitems/", Rest/binary>> ->
                    case binary:split(Rest, <<".ans104">>) of
                        [ID, <<>>] -> {true, ID};
                        _ -> false
                    end;
                _ -> false
            end
        end,
        Objects
    ).

%% @doc Filter dataitems based on gql query arguments
filter_dataitems(DataitemIDs, Args, Opts) ->
    lists:filter(
        fun(ID) ->
            matches_args(ID, Args, Opts)
        end,
        DataitemIDs
    ).

%% @doc Check if a dataitem matches the query arguments
matches_args(ID, Args, Opts) ->
    % Check if ID matches any specified IDs
    case maps:get(<<"ids">>, Args, undefined) of
        undefined -> true;
        IDs when is_list(IDs) -> lists:member(ID, IDs);
        SingleID -> ID == SingleID
    end
    andalso
    % Check tag matches (if specified int he query)
    case maps:get(<<"tags">>, Args, undefined) of
        undefined -> true;
        Tags -> matches_tags(ID, Tags, Opts)
    end.

%% @doc Check if a dataitem matches tag criteria
matches_tags(ID, Tags, Opts) ->
    case hb_gateway_s3:read(ID, Opts) of
        {ok, Message} ->
            TagMap = dev_query_graphql:keys_to_template(Tags),
            message_matches_tags(Message, TagMap, Opts);
        {error, _} ->
            false
    end.

%% @doc Check if a message matches the tag criteria
message_matches_tags(Message, TagMap, Opts) ->
    maps:fold(
        fun(TagName, TagValue, Acc) ->
            case hb_maps:get(TagName, Message, not_found, Opts) of
                TagValue -> Acc;
                _ -> false
            end
        end,
        true,
        TagMap
    ).

%% @doc Filter dataitems by tag map
filter_by_tags(DataitemIDs, TagMap, Opts) ->
    lists:filtermap(
        fun(ID) ->
            case hb_gateway_s3:read(ID, Opts) of
                {ok, Message} ->
                    case message_matches_tags(Message, TagMap, Opts) of
                        true -> {true, ID};
                        false -> false
                    end;
                {error, _} ->
                    false
            end
        end,
        DataitemIDs
    ).

%% @doc Load S3 messages for the given IDs
load_s3_messages(IDs, Opts) ->
    Messages = lists:filtermap(
        fun(ID) ->
            case hb_gateway_s3:read(ID, Opts) of
                {ok, Message} -> 
                    % Convert HyperBEAM message format to gql transaction format
                    GraphQLMessage = convert_to_graphql_format(Message, Opts),
                    {true, GraphQLMessage};
                {error, _} -> false
            end
        end,
        IDs
    ),
    {ok, Messages}.


%% @doc Convert HyperBEAM message format to gql transaction format
%% this function just ensure gql data fmt compatibility
convert_to_graphql_format(Message, Opts) ->
    ID = hb_maps:get(<<"id">>, Message, <<"unknown">>, Opts),
    Owner = hb_maps:get(<<"owner">>, Message, <<>>, Opts),
    Data = hb_maps:get(<<"data">>, Message, <<>>, Opts),
    
    % Convert message keys except sys keys to gql tags format
    SystemKeys = [<<"id">>, <<"owner">>, <<"signature">>, <<"data">>],
    Tags = maps:fold(
        fun(Key, Value, Acc) ->
            case lists:member(Key, SystemKeys) of
                true -> Acc;  % Skip system keys
                false ->
                    [#{<<"name">> => ensure_string(Key), <<"value">> => ensure_string(Value)} | Acc]
            end
        end,
        [],
        Message
    ),
    
    % Build minimal gql transaction format
    #{
        <<"id">> => ensure_string(ID),
        <<"anchor">> => null,
        <<"signature">> => ensure_string(hb_maps:get(<<"signature">>, Message, <<>>, Opts)),
        <<"recipient">> => <<"">>,
        <<"owner">> => #{
            <<"address">> => <<"">>,
            <<"key">> => ensure_string(Owner)
        },
        <<"fee">> => #{<<"winston">> => <<"0">>, <<"ar">> => <<"0">>},
        <<"quantity">> => #{<<"winston">> => <<"0">>, <<"ar">> => <<"0">>},
        <<"data">> => #{
            <<"size">> => integer_to_binary(byte_size(Data)),
            <<"type">> => extract_content_type(Tags)
        },
        <<"tags">> => Tags,
        <<"ingested_at">> => null,
        <<"block">> => null,
        <<"bundledIn">> => null
    }.

%% @doc Ensure a value is a string/binary for gql compatibility
ensure_string(Value) when is_binary(Value) -> Value;
ensure_string(Value) when is_atom(Value) -> atom_to_binary(Value, utf8);
ensure_string(Value) when is_integer(Value) -> integer_to_binary(Value);
ensure_string(Value) when is_float(Value) -> float_to_binary(Value);
ensure_string(Value) when is_list(Value) -> list_to_binary(Value);
ensure_string(_) -> <<"">>.

%% @doc Extract content-type from tags for the data mime type field
extract_content_type(Tags) ->
    case lists:keyfind(<<"content-type">>, 2, 
                      [{maps:get(<<"name">>, Tag), maps:get(<<"value">>, Tag)} || Tag <- Tags]) of
        {_, ContentType} -> ContentType;
        false -> null
    end.
