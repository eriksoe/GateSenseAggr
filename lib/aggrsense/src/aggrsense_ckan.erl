-module (aggrsense_ckan).

-export ([create_resource/2]).
-export ([create_resource/4]).
-export ([insert/5]).
-export ([run/0]).

create_resource(ResourceName, Data) ->
    {ok, IP} = application:get_env(aggrsense, ckan_server),
    {ok, ApiKey} = application:get_env(aggrsense, ckan_api_key),
    {ok, DataSet} = application:get_env(aggrsense, ckan_dataset),
    {ok, ResourceID} = create_resource(IP, DataSet, ApiKey, ResourceName),

    Fields = [
              {[{"id", <<"sensor">>}, {"type", <<"text">>}]},
              {[{"id", <<"time">>}, {"type", <<"integer">>}]},
              {[{"id", <<"value">>}, {"type", <<"float">>}]}
             ],
    create_table(ResourceID, IP, ApiKey, Fields),
    insert(ResourceID,Data,IP,DataSet, ApiKey).

run() ->
	Fields = [
         {[{"id", <<"name">>}, {"type", <<"text">>}]}, 
         {[{"id", <<"value">>}, {"type", <<"float">>}]}
        ],
	Data = [{[{"name", <<"egg1">>}, {"value", <<"1234">>}]}],
	% JsonObj = {[{"name", <<"egg1">>}, {"test", <<"1234">>}]},
	% Data = lists:flatten(mochijson2:encode(JsonObj)),
	IP = "54.246.105.104",
	DataSet = "aqe-aggr",
	
	%% ResourceID = create_resource(IP, DataSet, ApiKey, "Test"),
	%% create_table(ResourceID, IP, ApiKey, Fields),
	%% insert(ResourceID,Data,IP,DataSet, ApiKey).	
    'DUMMY'.

create_resource(IP, DataSet, ApiKey, ResourceName) ->
  % IP2="127.0.0.1:8080",
	URL = "http://" ++ IP ++ "/api/3/action/resource_create",
	Method = post,
	Headers = [{"X-CKAN-API-Key", ApiKey}],
	Type = "application/json",
	RequestBody = {[
					{"package_id", iolist_to_binary(DataSet)}, 
					{"url", <<"/area">>},
					{"name", iolist_to_binary(ResourceName)}]},
	JsonBody = iolist_to_binary(mochijson2:encode(RequestBody)),
	% Body = "{\"resource_id\" :\"" ++ ResourceName ++ "\", " ++ Data ++ "}",
	io:format("Body=~s\n", [JsonBody]),
	case httpc:request(Method, {URL, Headers, Type, JsonBody}, [], []) of
		{ok, {{_,200,_}, _Headers, Body}} ->
            handle_create_resource_results(Body);
        {ok, {{_,ErrCode,_}, _Headers, Body}} ->
            error_logger:info_msg("~s: Fetched failed for - http-code ~p: ~p\n",
                                  [?MODULE, ErrCode, Body]);
        {error, Error} ->
            error_logger:warning_msg("~s: Fetch failed ~p\n",
                                     [?MODULE, Error])
    end.

handle_create_resource_results(Json) ->
	% io:format("~p\n", [Json]),
	Spec = {object, [{<<"result">>,
  	{object, [{<<"id">>, string}, {keep_rest, fun(_) -> ignore end}]}},
    {keep_rest, fun(_) -> ignore end}]},
 	% Spec = {object, [{<<"id">>, string}, {keep_rest, fun(_) -> ignore end}]},
	{ok, {{ResourceID, _},_}} = aggrsense_util:parse_and_validate_json(Json, Spec),
	{ok, ResourceID}.

create_table(ResourceID, IP, ApiKey, Fields) ->
	URL = "http://" ++ IP ++ "/api/3/action/datastore_create",
	Method = post,
	Headers = [{"X-CKAN-API-Key", ApiKey}],
	Type = "application/json",
	Body = {[
					{"resource_id", ResourceID}, 
					{"fields", Fields }
					]},
	JsonBody = iolist_to_binary(mochijson2:encode(Body)),
	% Body = "{\"resource_id\" :\"" ++ ResourceName ++ "\", " ++ Data ++ "}",
	io:format("Body=~s\n", [JsonBody]),
	R = httpc:request(Method, {URL, Headers, Type, JsonBody}, [], []),
	{ok, {{"HTTP/1.1",200, State}, Head, ResponseBody}} = R.

insert(ResourceName, Data, IP, Dataset, ApiKey) ->
	URL = "http://" ++ IP ++ "/api/3/action/datastore_upsert",
	Method = post,
	Headers = [{"X-CKAN-API-Key", ApiKey}],
	Type = "application/json",
	Body = {[
					{"resource_id", iolist_to_binary(ResourceName)}, 
					{"records", Data },
					{"method", <<"insert">>}]},
	JsonBody = iolist_to_binary(mochijson2:encode(Body)),
	% Body = "{\"resource_id\" :\"" ++ ResourceName ++ "\", " ++ Data ++ "}",
	R = httpc:request(Method, {URL, Headers, Type, JsonBody}, [], []),
	{ok, {{"HTTP/1.1",200, State}, Head, ResponseBody}} = R.



	% URL = "http://api.cosm.com/v2/feeds/"++integer_to_list(FeedID),
 %    Headers = [{"X-ApiKey", ApiKey}],
 %    case httpc:request(get, {URL, Headers}, [], []) of
 %        {ok, {{_,200,_}, _Headers, Body}} ->
 %            %% error_logger:info_msg("~s: Fetched ~p: ~p\n",
 %            %%                       [?MODULE, FeedID, Body]),
 %            handle_results(FeedID, Body);
 %        {ok, {{_,ErrCode,_}, _Headers, Body}} ->
 %            error_logger:info_msg("~s: Fetched failed for ~p - http-code ~p: ~p\n",
 %                                  [?MODULE, FeedID, ErrCode, Body]);
 %        {error, Error} ->
 %            error_logger:warning_msg("~s: Fetch failed for ~p: ~p\n",
 %                                     [?MODULE, FeedID, Error])
 %    end.
