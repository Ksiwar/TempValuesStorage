-module(storage_worker).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(CLEANUP_INTERVAL, 30000). 
% TODO: maybe rewrite onto timer:apply_after(30 * 60 * 1000, storage, delete, [Key]).

-record(state, {
    table :: atom()
}).

%%% API
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%% gen_server cb's
init([]) ->
    Table = storage:get_table(), 
    timer:send_interval(?CLEANUP_INTERVAL, cleanup),
    {ok, #state{table = Table}}.

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(cleanup, State = #state{table = Table}) ->
    Now = erlang:system_time(second),
    ThirtyMinutesAgo = Now - 1800, 
    MatchSpec = [
        {
            {'$1', '_', '_', '$2'}, % Key, UserId, Value, CreatedAt
            [{'=<', '$2', ThirtyMinutesAgo}], % CreatedAt <= ThirtyMinutesAgo
            ['$1'] % del keys
        }
    ],
    KeysToDelete = ets:select(Table, MatchSpec),
    lists:foreach(fun(Key) -> ets:delete(Table, Key) end, KeysToDelete),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.