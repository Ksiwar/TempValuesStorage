-module(temp_storage_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Children = [
        {storage, {storage, start_link, []}, permanent, 5000, worker, [storage]},
        {storage_worker, {storage_worker, start_link, []}, permanent, 5000, worker, [storage_worker]},
        {web_api, {web_api, start_link, []}, permanent, 5000, worker, [web_api]}
    ],
    {ok, {#{strategy => one_for_one}, Children}}.