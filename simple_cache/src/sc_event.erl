-module(sc_event).

-behaviour(gen_event).

%% API
-export([
         start_link/0,
         add_handler/2,
         delete_handler/2,
         lookup/1,
         create/2,
         replace/2,
         delete/1
        ]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {}).

%%% ==================================================================
%%% API
%%% ==================================================================
start_link() ->
    gen_event:start_link({local, ?MODULE}).

add_handler(Handler, Args) ->
    gen_event:add_handler(?MODULE, Handler, Args).

delete_handler(Handler, Args) ->
    gen_event:delete_handler(?MODULE, Handler, Args).

lookup(Key) ->
    gen_event:notify(?MODULE, {lookup, Key}).

create(Key, Value) ->
    gen_event:notify(?MODULE, {create, {Key, Value}}).

replace(Key, Value) ->
    gen_event:notify(?MODULE, {replace, {Key, Value}}).

delete(Key) ->
    gen_event:notify(?MODULE, {delete, Key}).

%%% ==================================================================
%%% gen_server callbacks
%%% ==================================================================

init(_Arg) ->
    {ok, #state{}}.

handle_event(_Event, State) ->
    {ok, State}.

handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

handle_info(_Request, State) ->
    {ok, State}.

terminate(_Reson, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% ==================================================================
%%% Internal functions
%%% ==================================================================
