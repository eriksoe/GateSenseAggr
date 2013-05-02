
-module(aggrsense_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    ChildSpec =
        [{aggrsense_yaws_sup, {aggrsense_yaws_sup, start_link, []},
          permanent, 120000, worker, [aggrsense_yaws_sup]},
         {aggrsense_fetcher, {aggrsense_fetcher, start_link, []},
          permanent, 120000, worker, [aggrsense_fetcher]},
         {aggrsense_storage, {aggrsense_storage, start_link, []},
          permanent, 120000, worker, [aggrsense_storage]}
        ],
    {ok, { {one_for_one, 5, 10}, ChildSpec} }.

