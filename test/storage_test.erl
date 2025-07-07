-module(storage_test).
-include_lib("eunit/include/eunit.hrl").

setup() ->
    {ok, Pid} = storage:start_link(),
    Pid.

cleanup(_Pid) ->
    gen_server:stop(storage).

storage_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      fun test_create_get/1,
      fun test_update/1,
      fun test_delete/1,
      fun test_list/1,
      fun test_auth_check/1
     ]}.

test_create_get(_) ->
    ?assertEqual({ok, created}, storage:create(<<"k1">>, <<"u1">>, #{})),
    ?assertMatch({ok, #{}}, storage:get(<<"k1">>, <<"u1">>)).

test_update(_) ->
    storage:create(<<"k1">>, <<"u1">>, #{}),
    ?assertEqual({ok, updated}, storage:update(<<"k1">>, <<"u1">>, #{new => true})),
    ?assertMatch({ok, #{new := true}}, storage:get(<<"k1">>, <<"u1">>)).

test_delete(_) ->
    storage:create(<<"k1">>, <<"u1">>, #{}),
    ?assertEqual({ok, deleted}, storage:delete(<<"k1">>, <<"u1">>)),
    ?assertEqual({error, not_found}, storage:get(<<"k1">>, <<"u1">>)).

test_list(_) ->
    storage:create(<<"k1">>, <<"u1">>, #{a => 1}),
    storage:create(<<"k2">>, <<"u1">>, #{b => 2}),
    ?assertMatch({ok, [#{key := <<"k1">>}, #{key := <<"k2">>}]}, storage:list(<<"u1">>)).

test_auth_check(_) ->
    ?assertEqual({error, forbidden}, storage:get(<<"k1">>, <<"u2">>)).