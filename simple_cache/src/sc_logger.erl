-module(sc_logger).

-behaviour(gen_event).

%% API
-export([enable/0, disable/0]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {}).

%%% ==================================================================
%%% API
%%% ==================================================================
enable() ->
    sc_event:add_handler(?MODULE, []).

disable() ->
    sc_event:delete_handler(?MODULE, []).

%%% ==================================================================
%%% gen_server callbacks
%%% ==================================================================

init(_Arg) ->
    {ok, #state{}}.

handle_event({lookup, Key}, State) ->
    io:fwrite("LOOKUP ~p~n", [Key]),
    {ok, State};
handle_event({delete, Key}, State) ->
    io:fwrite("DELETE ~p~n", [Key]),
    {ok, State};
handle_event({create, {Key, Value}}, State) ->
    io:fwrite("CREATE ~p => ~p~n", [Key, Value]),
    {ok, State};
handle_event({replace, {Key, Value}}, State) ->
    io:fwrite("REPLACE ~p => ~p~n", [Key, Value]),
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
