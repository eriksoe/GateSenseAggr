{"resource_id":"79520cb9-a2b3-4f63-a64b-0f3db38c6620","records":[{"name":"egg1","test":"1234"}],"method":"insert"}

-module (aggrsense_ckan).

-export ([create_resource/2]).
-export ([create_resource/5]).
-export ([run/0]).

create_resource(ResourceName, Data) ->
    {ok, CKanIP} = application:get_env(aggrsense, ckan_server),
    {ok, CKanApiKey} = application:get_env(aggrsense, ckan_api_key),
    {ok, CKanDataset} = application:get_env(aggrsense, ckan_dataset),
    'TODO'.

run() ->
	Data = [{[{"name", <<"egg1">>}, {"test", <<"1234">>}]}],
	% JsonObj = {[{"name", <<"egg1">>}, {"test", <<"1234">>}]},
	% Data = lists:flatten(mochijson2:encode(JsonObj)),
	

create_resource(ResourceName, Data, IP, Dataset, ApiKey) ->
	% IP2="127.0.0.1:8080",
	URL = "http://" ++ IP ++ "/api/3/action/datastore_upsert",
	Method = post,
	Headers = [{"X-CKAN-API-Key", ApiKey}],
	Type = "application/json",
	Body = {[
					{"resource_id", list_to_binary(ResourceName)}, 
					{"records", Data },
					{"method", <<"insert">>}]},
	JsonBody = lists:flatten(mochijson2:encode(Body)),
	% Body = "{\"resource_id\" :\"" ++ ResourceName ++ "\", " ++ Data ++ "}",
	io:format("Body=~s\n", [JsonBody]),
	R = httpc:request(Method, {URL, Headers, Type, JsonBody}, [], []),
	{ok, {{"HTTP/1.1",200, State}, Head, Body}} = R.



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