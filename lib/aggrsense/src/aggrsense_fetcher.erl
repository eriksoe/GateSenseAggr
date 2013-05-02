%%%-------------------------------------------------------------------
%%% @author Erik Søe Sørensen <erik@flitwick>
%%% @copyright (C) 2013, Erik Søe Sørensen
%%% @doc
%%%
%%% @end
%%% Created :  2 May 2013 by Erik Søe Sørensen <erik@flitwick>
%%%-------------------------------------------------------------------
-module(aggrsense_fetcher).

-behaviour(gen_server).

-include_lib("eunit/include/eunit.hrl").

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {cosm_api_key, feed_ids}).

-define(ignore_rest, {keep_rest, fun(_)->rest end}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    {ok, ApiKey} = application:get_env(aggrsense, cosm_api_key),
    {ok, FeedIDs} = application:get_env(aggrsense, feeds),
    gen_server:start_link({local, ?SERVER}, ?MODULE, {ApiKey,FeedIDs}, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init({ApiKey,FeedIDs}) ->
    {ok, _} = timer:send_interval(1 * 60 * 1000, fetch),
    self() ! fetch,
    %% {ok, _HttpcPid} = inet:start(httpc, [{profile, ?MODULE}]),
    {ok, #state{cosm_api_key=ApiKey,
                feed_ids=FeedIDs}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    error_logger:info_msg("~s: fetching data from feeds\n", [?MODULE]),
    fetch_all_feeds(State),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

fetch_all_feeds(#state{feed_ids=FeedIDs}=State) ->
    lists:foreach(fun(ID) -> fetch_feed(ID, State) end,
                  FeedIDs).

fetch_feed(FeedID, #state{cosm_api_key=ApiKey}) when is_integer(FeedID) ->
    URL = "http://api.cosm.com/v2/feeds/"++integer_to_list(FeedID),
    Headers = [{"X-ApiKey", ApiKey}],
    case httpc:request(get, {URL, Headers}, [], []) of
        {ok, {{_,200,_}, _Headers, Body}} ->
            %% error_logger:info_msg("~s: Fetched ~p: ~p\n",
            %%                       [?MODULE, FeedID, Body]),
            handle_results(FeedID, Body);
        {ok, {{_,ErrCode,_}, _Headers, Body}} ->
            error_logger:info_msg("~s: Fetched failed for ~p - http-code ~p: ~p\n",
                                  [?MODULE, FeedID, ErrCode, Body]);
        {error, Error} ->
            error_logger:warning_msg("~s: Fetch failed for ~p: ~p\n",
                                     [?MODULE, FeedID, Error])
    end.

handle_results(FeedID, Json) ->
    %% TODO: Extract location
    DisplayResult =
        case parse_and_validate_json(Json) of
            {error,_}=ValidationResult -> ValidationResult;
            {ok, Validated} ->
                extract_data(Validated)
        end,
    error_logger:info_msg("Reading from ~p: ~p\n", [FeedID, DisplayResult]).

parse_and_validate_json(JsonStr) ->
    Spec = {object, [{<<"updated">>, string},
                     {<<"datastreams">>, {array,
                                          {object, [{<<"id">>, string},
                                                    {<<"at">>, string},
                                                    {<<"tags">>, {array, string}},
                                                    {<<"unit">>, {object, [{<<"symbol">>, string}, ?ignore_rest]}},
                                                    {<<"current_value">>, string},
                                                    ?ignore_rest]}}},
                     ?ignore_rest]},
    parse_and_validate_json(JsonStr, Spec).

parse_and_validate_json(JsonStr, Spec) ->
    try mochijson2:decode(JsonStr) of
        JsonValue ->
            case valijate:validate(JsonValue, Spec) of
                {ok, _}=Result -> Result;
                {validation_error, _,_}=VErr ->
                    {error, valijate:error_to_english(VErr)}
            end
    catch
        _:_PErr ->
            %% Too bad that mochijson2 doesn't give error descriptions.
            {error, "Error in JSON syntax"}
    end.

extract_data({_Timestamp, Measurements0,rest}) ->
    Spec = [{<<"Dust">>, <<"pcs/283ml">>},
            {<<"CO">>, <<"ppb">>},
            {<<"Humidity">>, <<"%">>},
            {<<"NO2">>, <<"ppb">>},
            {<<"O3">>, <<"ppb">>},
            {<<"Temperature">>, <<"deg C">>},
            {<<"VOC">>, <<"ppm">>}],
    Measurements =
        [{MesName, Time, string_to_float(Value)}
         || {_Id,Time,Tags,{Unit,rest},Value,rest} <- Measurements0,
            {MesName, MesUnit} <- Spec,
            Unit == MesUnit,
            lists:member(<<"aqe:sensor_type=", MesName/binary>>, Tags)
            %% binary:prefix(<<MesName/binary, "_">>, Id)
        ],
    Measurements.

%% binary_prefix(Prefix, Subject) ->
%%     PLen = byte_size(Prefix),
%%     byte_size(Subject) >= PLen andalso binary:part(Subject,0,PLen)==Prefix.

string_to_float(S) when is_binary(S) ->
    string_to_float(binary_to_list(S));
string_to_float(S) ->
    try list_to_integer(S) of
        V -> round(V)
    catch error:badarg ->
        list_to_float(S)
    end.

-ifdef(TEST).
-define(with_trace(Body),
    try (Body)
    catch
        Cls:Err ->
            Trace = erlang:get_stacktrace(),
            error_logger:error_msg("Test function threw: ~s:~p\n** Trace: ~p\n", [Cls, Err, Trace]),
            erlang:raise(Cls, Err, Trace)
    end).

extractor_test() ->
?with_trace(begin
    application:start(sasl),
    Json = "{\"id\":115602,\"title\":\"Mt. Jalapeño\",\"private\":\"false\",\"tags\":[\"device:type=Air Quality Egg\",\"device:type=airqualityegg\"],\"description\":\"Egg@erkkila.org\",\"feed\":\"https://api.cosm.com/v2/feeds/115602.json\",\"status\":\"live\",\"updated\":\"2013-05-02T12:20:56.491692Z\",\"created\":\"2013-02-28T15:42:59.205526Z\",\"creator\":\"https://cosm.com/users/airqualityegg\",\"version\":\"1.0.0\",\"datastreams\":[{\"id\":\"CO_00-04-a3-37-cc-7f_1\",\"current_value\":\"15289\",\"at\":\"2013-05-02T12:20:51.178061Z\",\"max_value\":\"16500.0\",\"min_value\":\"5212.0\",\"tags\":[\"aqe:data_origin=computed\",\"aqe:firmware_version=19\",\"aqe:sensor_address=00:04:a3:37:cc:7f\",\"aqe:sensor_index=1\",\"aqe:sensor_type=CO\"],\"unit\":{\"symbol\":\"ppb\",\"label\":\"ppb\"}},{\"id\":\"CO_raw_00-04-a3-37-cc-7f_1\",\"current_value\":\"112500\",\"at\":\"2013-05-02T12:20:46.039326Z\",\"max_value\":\"199799.0\",\"min_value\":\"36600.0\",\"tags\":[\"aqe:data_origin=raw\",\"aqe:firmware_version=19\",\"aqe:sensor_address=00:04:a3:37:cc:7f\",\"aqe:sensor_index=1\",\"aqe:sensor_type=CO\"],\"unit\":{\"symbol\":\"ohms\",\"label\":\"ohms\"}},{\"id\":\"Dust_00-04-a3-37-dd-08_0\",\"current_value\":\"0\",\"at\":\"2013-05-02T12:19:21.700657Z\",\"max_value\":\"6338.0\",\"min_value\":\"0.0\",\"tags\":[\"aqe:data_origin=computed\",\"aqe:firmware_version=16\",\"aqe:sensor_address=00:04:a3:37:dd:08\",\"aqe:sensor_index=0\",\"aqe:sensor_type=Dust\"],\"unit\":{\"symbol\":\"pcs/283ml\",\"label\":\"pcs/283ml\"}},{\"id\":\"Dust_raw_00-04-a3-37-dd-08_0\",\"current_value\":\"0\",\"at\":\"2013-05-02T12:19:16.666927Z\",\"max_value\":\"10559.0\",\"min_value\":\"0.0\",\"tags\":[\"aqe:data_origin=raw\",\"aqe:firmware_version=16\",\"aqe:sensor_address=00:04:a3:37:dd:08\",\"aqe:sensor_index=0\",\"aqe:sensor_type=Dust\"],\"unit\":{\"symbol\":\"ohms\",\"label\":\"ohms\"}},{\"id\":\"Humidity_00-04-a3-37-bb-24_1\",\"current_value\":\"33\",\"at\":\"2013-05-02T12:20:30.728794Z\",\"max_value\":\"54.0\",\"min_value\":\"17.0\",\"tags\":[\"aqe:data_origin=computed\",\"aqe:firmware_version=4\",\"aqe:sensor_address=00:04:a3:37:bb:24\",\"aqe:sensor_index=1\",\"aqe:sensor_type=Humidity\"],\"unit\":{\"symbol\":\"%\",\"label\":\"%\"}},{\"id\":\"NO2_00-04-a3-37-cc-7f_0\",\"current_value\":\"1124\",\"at\":\"2013-05-02T12:20:40.970205Z\",\"max_value\":\"1552.0\",\"min_value\":\"1.0\",\"tags\":[\"aqe:data_origin=computed\",\"aqe:firmware_version=19\",\"aqe:sensor_address=00:04:a3:37:cc:7f\",\"aqe:sensor_index=0\",\"aqe:sensor_type=NO2\"],\"unit\":{\"symbol\":\"ppb\",\"label\":\"ppb\"}},{\"id\":\"NO2_raw_00-04-a3-37-cc-7f_0\",\"current_value\":\"892669\",\"at\":\"2013-05-02T12:20:35.830248Z\",\"max_value\":\"1301368.0\",\"min_value\":\"726.0\",\"tags\":[\"aqe:data_origin=raw\",\"aqe:firmware_version=19\",\"aqe:sensor_address=00:04:a3:37:cc:7f\",\"aqe:sensor_index=0\",\"aqe:sensor_type=NO2\"],\"unit\":{\"symbol\":\"ohms\",\"label\":\"ohms\"}},{\"id\":\"O3_00-04-a3-37-bd-41_0\",\"current_value\":\"1211\",\"at\":\"2013-05-02T12:19:01.405914Z\",\"max_value\":\"2390.0\",\"min_value\":\"80.0\",\"tags\":[\"aqe:data_origin=computed\",\"aqe:firmware_version=16\",\"aqe:sensor_address=00:04:a3:37:bd:41\",\"aqe:sensor_index=0\",\"aqe:sensor_type=O3\"],\"unit\":{\"symbol\":\"ppb\",\"label\":\"ppb\"}},{\"id\":\"O3_raw_00-04-a3-37-bd-41_0\",\"current_value\":\"115899\",\"at\":\"2013-05-02T12:20:56.258637Z\",\"max_value\":\"189899.0\",\"min_value\":\"8999.0\",\"tags\":[\"aqe:data_origin=raw\",\"aqe:firmware_version=16\",\"aqe:sensor_address=00:04:a3:37:bd:41\",\"aqe:sensor_index=0\",\"aqe:sensor_type=O3\"],\"unit\":{\"symbol\":\"ohms\",\"label\":\"ohms\"}},{\"id\":\"Temperature_00-04-a3-37-bb-24_0\",\"current_value\":\"20\",\"at\":\"2013-05-02T12:20:25.457899Z\",\"max_value\":\"25.0\",\"min_value\":\"18.0\",\"tags\":[\"aqe:data_origin=computed\",\"aqe:firmware_version=4\",\"aqe:sensor_address=00:04:a3:37:bb:24\",\"aqe:sensor_index=0\",\"aqe:sensor_type=Temperature\"],\"unit\":{\"symbol\":\"deg C\",\"label\":\"deg C\"}},{\"id\":\"VOC_00-04-a3-37-b3-ec_0\",\"current_value\":\"-10142\",\"at\":\"2013-05-02T12:19:11.631008Z\",\"max_value\":\"28.0\",\"min_value\":\"-16741.0\",\"tags\":[\"aqe:data_origin=computed\",\"aqe:firmware_version=16\",\"aqe:sensor_address=00:04:a3:37:b3:ec\",\"aqe:sensor_index=0\",\"aqe:sensor_type=VOC\"],\"unit\":{\"symbol\":\"ppm\",\"label\":\"ppm\"}},{\"id\":\"VOC_raw_00-04-a3-37-b3-ec_0\",\"current_value\":\"268250\",\"at\":\"2013-05-02T12:19:06.508500Z\",\"max_value\":\"427600.0\",\"min_value\":\"4900.0\",\"tags\":[\"aqe:data_origin=raw\",\"aqe:firmware_version=16\",\"aqe:sensor_address=00:04:a3:37:b3:ec\",\"aqe:sensor_index=0\",\"aqe:sensor_type=VOC\"],\"unit\":{\"symbol\":\"ohms\",\"label\":\"ohms\"}}],\"location\":{\"exposure\":\"indoor\",\"domain\":\"physical\",\"ele\":\"642\",\"lat\":43.0695827,\"lon\":-77.1999378}}",
    {ok, Validated} = parse_and_validate_json(Json),

    ?assertMatch([{<<"CO">>,          _, 15289},
                  {<<"Dust">>,        _, 0},
                  {<<"Humidity">>,    _, 33},
                  {<<"NO2">>,         _, 1124},
                  {<<"O3">>,          _, 1211},
                  {<<"Temperature">>, _, 20},
                  {<<"VOC">>,         _, -10142}],
                 lists:sort(extract_data(Validated)))
end).

-endif.

