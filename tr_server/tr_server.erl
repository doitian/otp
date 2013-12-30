% @doc TCP RPC server.
-module(tr_server).
-behaviour(gen_server).

%% API
-export([start_link/1, start_link/0, get_count/0, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("eunit/include/eunit.hrl").

-record(state, {
          port :: integer(),
          request_count = 0 :: integer(),
          lsock  %socket()
         }).

-define(DEFAULT_PORT, 3000).
-define(SERVER, ?MODULE).

%%%==================================================
%%% API
%%%==================================================

% @doc Starts the server.
-spec(start_link(Port :: integer()) -> {ok, pid()}).
start_link(Port) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Port], []).

% @doc Starts the server with default port.
-spec(start_link() -> {ok, pid()}).
start_link() ->
    start_link(?DEFAULT_PORT).

% @doc Gets RPC calls count.
-spec(get_count() -> integer()).
get_count() ->
    gen_server:call(?SERVER, get_count).

% @doc Stop the server.
-spec(stop() -> ok).
stop() ->
    gen_server:cast(?SERVER, stop).

%%%==================================================
%%% gen_server callbacks
%%%==================================================

init([Port]) ->
    {ok, LSock} = gen_tcp:listen(Port, [{active, true}]),
    {ok, #state{port = Port, lsock = LSock}, 0}.

handle_call(get_count, _From, State) ->
    {reply, {ok, State#state.request_count}, State}.

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info(timeout, #state{lsock = LSock} = State) ->
    {ok, _Sock} = gen_tcp:accept(LSock),
    {noreply, State};
handle_info({tcp, Socket, RawData}, #state{request_count = Count} = State) ->
    reply(Socket, eval(RawData)),
    {noreply, State#state{request_count = Count + 1}}.

terminate(_Reson, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%==================================================
%%% Internal functions
%%%==================================================

% @doc Send result back through Socket.
reply(Socket, Result) ->
    gen_tcp:send(Socket, io_lib:fwrite("~p~n", [Result])).

% @doc Evaluate request data and send back result.
eval(RawData) ->
    try
        {M, F, A} = split_out_mfa(RawData),
        {ok, apply(M, F, A)}
    catch
        _Class:Err ->
            Trace = erlang:get_stacktrace(),
            {error, Err, Trace}
    end.

split_out_mfa(RawData) ->
    MFA = re:replace(RawData, "\r\n$", "", [{return, list}]),
    {match, [M, F, A]} =
        re:run(MFA,
               "(.*):(.*)\s*\\((.*)\s*\\)\s*\\.\s*$",
               [{capture, [1,2,3], list}, ungreedy]),
    {list_to_existing_atom(M), list_to_existing_atom(F), args_to_terms(A)}.

split_out_mfa_test_() ->
    [
     ?_assertEqual({lists, reverse, [[1,2,3]]}, split_out_mfa("lists:reverse([1,2,3])."))
    ].

args_to_terms(RawArgs) ->
    {ok, Toks, _Line} = erl_scan:string("[" ++ RawArgs ++ "]."),
    {ok, Args} = erl_parse:parse_term(Toks),
    Args.
