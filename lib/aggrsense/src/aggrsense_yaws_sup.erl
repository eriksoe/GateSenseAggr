
-module(aggrsense_yaws_sup).

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
    GconfList = [{logdir, "log/aggrsense"},
%                 {ebin_dir, ["/example1/ebin", "/example2/ebin"]},
                 {id, "aggrsense"}],
    
    Docroot = filename:join(code:priv_dir(aggrsense), "www"),
    SconfList = [{docroot, Docroot},
                 {port, 8080},
                 {listen, {127,0,0,1}},
                 {appmods, [{"/", aggrsense_appmod}]}],
    {ok, SCList, GC, ChildSpecs} =
        yaws_api:embedded_start_conf(Docroot, SconfList, GconfList),
    io:format("DB| yaws specs:\n** SCList=~p\n**GC=~p\n**CS=~p\n",
              [SCList, GC, ChildSpecs]),
    Res = supervisor:start_link({local, ?MODULE}, ?MODULE, {ChildSpecs}),
    yaws_api:setconf(GC, SCList),
    Res.

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init({ChildSpecs}) ->
    {ok, { {one_for_one, 5, 10}, ChildSpecs} }.

