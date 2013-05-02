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

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {cosm_api_key, feed_ids}).

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
        {error, Error} ->
            error_logger:warning_msg("~s: Fetch failed for ~p: ~p\n",
                                     [?MODULE, FeedID, Error]);
        {ok, {_StatusLine, _Headers, Body}} ->
            error_logger:info_msg("~s: Fetched ~p: ~p\n",
                                  [?MODULE, FeedID, Body])
    end.
