%%%-------------------------------------------------------------------
%%% @author Erik Søe Sørensen <erik@flitwick>
%%% @copyright (C) 2013, Erik Søe Sørensen
%%% @doc
%%%
%%% @end
%%% Created :  2 May 2013 by Erik Søe Sørensen <erik@flitwick>
%%%-------------------------------------------------------------------
-module(aggrsense_storage).

-behaviour(gen_server).

%% API
-export([start_link/0, add/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(measurement,
        {key,
         time,
         lon, lat, % Coordinates
         value
        }).
-record(state, {table}).

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
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

add(FeedID, Location, Measurements) ->
    gen_server:call(?SERVER, {add, FeedID, Location, Measurements}).

query_rect(X1, Y1, X2, Y2) ->
    gen_server:call(?SERVER, {query_rect, X1, Y1, X2, Y2}).

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
init([]) ->
    {ok, #state{
       table=ets:new(aggrsense_storage, [protected, {keypos, #measurement.key}])
      }}.

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
handle_call({add, FeedID, _Location={Lon,Lat,_,_}, Measurements}, _From,
            State=#state{table=Table})
  when is_list(Measurements) ->
    Objects = lists:map(fun({MesName, Time, Value}) ->
                                Key = {FeedID, MesName},
                                #measurement{key=Key,
                                             time=Time,
                                             lon=Lon,
                                             lat=Lat,
                                             value=Value}
                        end,
                  Measurements),
    true = ets:insert(Table, Objects),
    error_logger:info_msg("~s: ~p rows updated\n", [?MODULE, length(Measurements)]),
    {reply, ok, State};
handle_call({query_rect, X1, Y1, X2, Y2}, _From, State=#state{table=Table}) ->
    Result = 'TODO',
    {reply, Result, State};
handle_call(_Request, _From, State) ->
    {noreply, State}.

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
