-module(utils).
-export([validate_json/1, format_error/1]).

validate_json(Data) when is_map(Data) ->
    case maps:get(<<"key">>, Data, undefined) of
        undefined -> {error, missing_key};
        Key when is_binary(Key), byte_size(Key) > 0 ->
            case maps:get(<<"value">>, Data, undefined) of
                undefined -> {error, missing_value};
                Value when is_map(Value) -> ok;
                _ -> {error, invalid_value}
            end;
        _ -> {error, invalid_key}
    end.

format_error(Error) ->
    case Error of
        missing_key -> <<"Missing 'key' field">>;
        missing_value -> <<"Missing 'value' field">>;
        invalid_key -> <<"Key must be non-empty binary">>;
        invalid_value -> <<"Value must be JSON object">>;
        _ -> <<"Unknown error">>
    end.