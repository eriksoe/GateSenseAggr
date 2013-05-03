-module(aggrsense_util).
-export([parse_and_validate_json/2]).

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
