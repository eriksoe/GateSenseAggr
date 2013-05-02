-module(aggrsense_appmod).

-export([out/1]).

-include_lib("yaws/include/yaws_api.hrl").

-define(REQUIRE_GET(Met, Body),
        case (Met) of
            'GET' -> Body;
            _     -> [{status, 405}]
        end).

-define(PARSE_JSON(A, Spec, Var, Body),
        case parse_and_validate_json(yaws_api:arg_clidata(A), Spec) of
            {ok, Var} ->
                (Body);
            {error, XXErrorText} ->
                [{status, 400},
                 {content, "text/plain", [XXErrorText, "\n"]}]
        end).

out(A) ->
    Method = yaws_api:http_request_method(yaws_api:arg_req(A)),
    Uri = yaws_api:request_url(A),
    Path = string:tokens(Uri#url.path, "/"),
    dispatch(Method, Path, A).

%% General pages:
dispatch(Met, ["aggrsense"], A) ->
    ?REQUIRE_GET(Met, base_page(A));
dispatch(Met, ["aggrsense", "test"|_], A) ->
    ?REQUIRE_GET(Met, test_page(A));
dispatch(_, _, _) ->
    [{status, 404}].

base_page(_A) ->
    {ehtml,
     [{h1, [], "This is GateSenseAggr."},
      {p, [], "The REST interface is up and running."}
     ]}.

test_page(A) ->
    Uri = yaws_api:request_url(A),
    Path = string:tokens(Uri#url.path, "/"),
    Method = yaws_api:http_request_method(yaws_api:arg_req(A)),
    {ehtml,
     [{p, [], "Hello, World!"},
      {dl, [], [
      {dt, [], "Method:"}, {dd,[], io_lib:format("~p", [Method])},
      {dt, [], "Uri:"}, {dd,[], io_lib:format("~p", [Uri])},
      {dt, [], "Path:"}, {dd,[], io_lib:format("~p", [Path])}
               ]},
      {p, [], io_lib:format("~p", [A])}
     ]}.
