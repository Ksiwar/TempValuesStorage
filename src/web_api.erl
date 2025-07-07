-module(web_api).
-behaviour(gen_server).
-behaviour(cowboy_handler).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([init/2]).

%%% API
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%% Cowboy handler cb
init(Req0, State) ->
    try
        Method = cowboy_req:method(Req0),
        Path = cowboy_req:path(Req0),
        handle_request(Method, Path, Req0)
    catch
        _:Reason ->
            error_logger:error_msg("Request failed: ~p~n", [Reason]),
            reply(500, #{<<"error">> => <<"internal_server_error">>}, Req0)
    end,
    {ok, Req0, State}.

%%% gen_server cb's
init([]) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/keys", ?MODULE, []},
            {"/keys/:key", ?MODULE, []}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(
        http_listener,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
    ),
    {ok, #{}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    cowboy:stop_listener(http_listener),
    ok.

%%% request handling
handle_request(<<"POST">>, <<"/keys">>, Req0) ->
    case cowboy_req:has_body(Req0) of
        true ->
            case cowboy_req:read_body(Req0) of
                {ok, Body, Req1} ->
                    process_post_request(Body, Req1);
                {error, _Reason} ->
                    reply(400, #{<<"error">> => <<"bad_request">>}, Req0)
            end;
        false ->
            reply(400, #{<<"error">> => <<"missing_body">>}, Req0)
    end;

handle_request(<<"GET">>, <<"/keys">>, Req0) ->
    process_get_all_request(Req0);

handle_request(<<"GET">>, <<"/keys/", Key/binary>>, Req0) ->
    process_get_request(Key, Req0);

handle_request(<<"PUT">>, <<"/keys/", Key/binary>>, Req0) ->
    case cowboy_req:has_body(Req0) of
        true ->
            case cowboy_req:read_body(Req0) of
                {ok, Body, Req1} ->
                    process_put_request(Key, Body, Req1);
                {error, _Reason} ->
                    reply(400, #{<<"error">> => <<"bad_request">>}, Req0)
            end;
        false ->
            reply(400, #{<<"error">> => <<"missing_body">>}, Req0)
    end;

handle_request(<<"DELETE">>, <<"/keys/", Key/binary>>, Req0) ->
    process_delete_request(Key, Req0);

handle_request(_, _, Req) ->
    reply(405, #{<<"error">> => <<"method_not_allowed">>}, Req).

%%% helpers
process_post_request(Body, Req) ->
    AuthHeader = cowboy_req:header(<<"authorization">>, Req, <<>>),
    case auth:check(AuthHeader) of
        {ok, UserId} ->
            try jsx:decode(Body, [return_maps]) of
                #{<<"key">> := Key, <<"value">> := Value} ->
                    case storage:create(Key, UserId, Value) of
                        {ok, created} -> 
                            reply(201, #{<<"status">> => <<"created">>}, Req);
                        {error, already_exists} -> 
                            reply(409, #{<<"error">> => <<"key_exists">>}, Req)
                    end;
                _ ->
                    reply(400, #{<<"error">> => <<"invalid_json">>}, Req)
            catch
                _:_ -> 
                    reply(400, #{<<"error">> => <<"invalid_json">>}, Req)
            end;
        {error, Reason} ->
            reply(401, #{<<"error">> => <<"unauthorized">>, <<"reason">> => Reason}, Req)
    end.

process_get_all_request(Req) ->
    AuthHeader = cowboy_req:header(<<"authorization">>, Req, <<>>),
    case auth:check(AuthHeader) of
        {ok, UserId} ->
            {ok, List} = storage:list(UserId),
            reply(200, List, Req);
        {error, Reason} ->
            reply(401, #{<<"error">> => <<"unauthorized">>, <<"reason">> => Reason}, Req)
    end.

process_get_request(Key, Req) ->
    AuthHeader = cowboy_req:header(<<"authorization">>, Req, <<>>),
    case auth:check(AuthHeader) of
        {ok, UserId} ->
            case storage:get(Key, UserId) of
                {ok, Value} -> 
                    reply(200, Value, Req);
                {error, not_found} -> 
                    reply(404, #{<<"error">> => <<"not_found">>}, Req);
                {error, forbidden} -> 
                    reply(403, #{<<"error">> => <<"forbidden">>}, Req)
            end;
        {error, Reason} ->
            reply(401, #{<<"error">> => <<"unauthorized">>, <<"reason">> => Reason}, Req)
    end.

process_put_request(Key, Body, Req) ->
    AuthHeader = cowboy_req:header(<<"authorization">>, Req, <<>>),
    case auth:check(AuthHeader) of
        {ok, UserId} ->
            try jsx:decode(Body, [return_maps]) of
                #{<<"value">> := Value} ->
                    case storage:update(Key, UserId, Value) of
                        {ok, updated} -> 
                            reply(200, #{<<"status">> => <<"updated">>}, Req);
                        {error, not_found} -> 
                            reply(404, #{<<"error">> => <<"not_found">>}, Req);
                        {error, forbidden} -> 
                            reply(403, #{<<"error">> => <<"forbidden">>}, Req)
                    end;
                _ ->
                    reply(400, #{<<"error">> => <<"invalid_json">>}, Req)
            catch
                _:_ -> 
                    reply(400, #{<<"error">> => <<"invalid_json">>}, Req)
            end;
        {error, Reason} ->
            reply(401, #{<<"error">> => <<"unauthorized">>, <<"reason">> => Reason}, Req)
    end.

process_delete_request(Key, Req) ->
    AuthHeader = cowboy_req:header(<<"authorization">>, Req, <<>>),
    case auth:check(AuthHeader) of
        {ok, UserId} ->
            case storage:delete(Key, UserId) of
                {ok, deleted} -> 
                    reply(200, #{<<"status">> => <<"deleted">>}, Req);
                {error, not_found} -> 
                    reply(404, #{<<"error">> => <<"not_found">>}, Req);
                {error, forbidden} -> 
                    reply(403, #{<<"error">> => <<"forbidden">>}, Req)
            end;
        {error, Reason} ->
            reply(401, #{<<"error">> => <<"unauthorized">>, <<"reason">> => Reason}, Req)
    end.

reply(Status, Body, Req) ->
    cowboy_req:reply(
        Status,
        #{<<"content-type">> => <<"application/json">>},
        jsx:encode(Body),
        Req
    ).