%%%-------------------------------------------------------------------
%%% @author olegb
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Mar 2021 11:19
%%%-------------------------------------------------------------------
-module(simple_cache).
-author("olegb").

%% API
-export([insert/2, lookup/1, delete/1]).

insert(Key, Value) ->
  case simple_cache_store:lookup(Key) of
    {ok, Pid} ->
      simple_cache_event:replace(Key, Value),
      simple_cache_element:replace(Pid, Value);
    {error, _} ->
      {ok, Pid} = simple_cache_element:create(Value),
      simple_cache_store:insert(Key, Pid),
      simple_cache_event:create(Key, Value)
  end.

lookup(Key) ->
  simple_cache_event:lookup(Key),
  try
    {ok, Pid} = simple_cache_store:lookup(Key),
    {ok, Value} = simple_cache_element:fetch(Pid),
    {ok, Value}
  catch
    _Class:_Exception ->
      {error, not_found}
  end.

delete(Key) ->
  simple_cache_event:delete(Key),
  case simple_cache_store:lookup(Key) of
    {ok, Pid} ->
      simple_cache_element:delete(Pid);
    {error, _Reason} ->
      ok
  end.