-module(auth).
-export([check/1]).

check(undefined) -> {error, no_auth};
check(<<"Basic ", CredentialsBase64/binary>>) ->
    Credentials = base64:decode(CredentialsBase64),
    case binary:split(Credentials, <<":">>) of
        [UserId, _Password] -> 
            % TODO: add JWT or smth like that
            {ok, UserId};
        _ -> {error, invalid_format}
    end;
check(_) -> {error, invalid_auth}.