-module(storage).
-behaviour(gen_server).

-export([
    start_link/0,
    create/3,
    get/2,
    update/3,
    delete/2,
    list/1
]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([get_table/0]).
-record(state, {
    table_id :: ets:tid()
}).

%%% API
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

create(Key, UserId, Value) ->
    gen_server:call(?MODULE, {create, Key, UserId, Value}).

get(Key, UserId) ->
    gen_server:call(?MODULE, {get, Key, UserId}).

update(Key, UserId, NewValue) ->
    gen_server:call(?MODULE, {update, Key, UserId, NewValue}).

delete(Key, UserId) ->
    gen_server:call(?MODULE, {delete, Key, UserId}).

list(UserId) ->
    gen_server:call(?MODULE, {list, UserId}).

get_table() ->
    gen_server:call(?MODULE, get_table).

%%% Callbacks
init([]) ->
    TableId = ets:new(?MODULE, [
        named_table,
        set,
        protected,
        {keypos, 1},
        {read_concurrency, true}
    ]),
    {ok, #state{table_id = TableId}}.

handle_call({create, Key, UserId, Value}, _From, State = #state{table_id = TableId}) ->
    case ets:lookup(TableId, Key) of
        [] ->
            Now = erlang:system_time(second),
            ets:insert(TableId, {Key, UserId, Value, Now}),
            {reply, {ok, created}, State};
        [_] ->
            {reply, {error, already_exists}, State}
    end;

handle_call({get, Key, UserId}, _From, State = #state{table_id = TableId}) ->
    case ets:lookup(TableId, Key) of
        [{Key, UserId, Value, _}] -> {reply, {ok, Value}, State};
        [_] -> {reply, {error, forbidden}, State}; 
        [] -> {reply, {error, not_found}, State}
    end;

handle_call({update, Key, UserId, NewValue}, _From, State = #state{table_id = TableId}) ->
    case ets:lookup(TableId, Key) of
        [{Key, UserId, _, _}] ->
            Now = erlang:system_time(second),
            ets:insert(TableId, {Key, UserId, NewValue, Now}),
            {reply, {ok, updated}, State};
        [_] -> {reply, {error, forbidden}, State};
        [] -> {reply, {error, not_found}, State}
    end;

handle_call({delete, Key, UserId}, _From, State = #state{table_id = TableId}) ->
    case ets:lookup(TableId, Key) of
        [{Key, UserId, _, _}] ->
            ets:delete(TableId, Key),
            {reply, {ok, deleted}, State};
        [_] -> {reply, {error, forbidden}, State};
        [] -> {reply, {error, not_found}, State}
    end;

handle_call({list, UserId}, _From, State = #state{table_id = TableId}) ->
    Entries = ets:match(TableId, {'$1', UserId, '$2', '_'}),
    Result = lists:map(fun([Key, Value]) -> #{key => Key, value => Value} end, Entries),
    {reply, {ok, Result}, State};

handle_call(get_table, _From, State = #state{table_id = TableId}) ->
    {reply, TableId, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.