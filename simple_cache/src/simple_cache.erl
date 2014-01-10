-module(simple_cache).
-export([insert/2, lookup/1, delete/1]).

insert(Key, Value) ->
    case sc_store:lookup(Key) of
        {ok, Pid} ->
            sc_element:replace(Pid, Value);
        {error, _} ->
            {ok, Pid} = sc_element:create(Key, Value),
            sc_store:insert(Key, Pid)
    end.

lookup(Key) ->
    case sc_store:lookup(Key) of
        {ok, Pid} ->
            sc_element:fetch(Pid);
        {error, _} ->
            sc_event:lookup(Key),
            {error, not_found}
    end.

delete(Key) ->
    case sc_store:lookup(Key) of
        {ok, Pid} ->
            sc_element:delete(Pid);
        {error, _} ->
            ok
    end.

