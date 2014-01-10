-module(sc_element).

-behaviour(gen_server).

%% API
-export([
         start_link/3,
         create/3,
         create/2,
         fetch/1,
         replace/2,
         delete/1
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(DEFAULT_LEASE_TIME, (60 * 60 * 24)).

-record(state, {key, value, lease_time, start_time}).

%%% ==================================================================
%%% API
%%% ==================================================================

start_link(Key, Value, LeaseTime) ->
    gen_server:start_link(?MODULE, [Key, Value, LeaseTime], []).

create(Key, Value, LeaseTime) ->
    sc_element_sup:start_child(Key, Value, LeaseTime).
create(Key, Value) ->
    create(Key, Value, ?DEFAULT_LEASE_TIME).

fetch(Pid) ->
    gen_server:call(Pid, fetch).

replace(Pid, Value) ->
    gen_server:cast(Pid, {replace, Value}).

delete(Pid) ->
    gen_server:cast(Pid, delete).

%%% ==================================================================
%%% gen_server callbacks
%%% ==================================================================

init([Key, Value, LeaseTime]) ->
    StartTime = local_time_as_seconds(),
    State = #state{key = Key,
                   value = Value,
                   lease_time = LeaseTime,
                   start_time = StartTime},
    sc_event:create(Key, Value),
    {ok, State, time_left(StartTime, LeaseTime)}.

handle_call(fetch, _From, State) ->
    #state{key = Key, value = Value} = State,
    sc_event:lookup(Key),
    {reply, {ok, Value}, State, time_left(State)}.

handle_cast({replace, Value}, State) ->
    #state{key = Key} = State,
    sc_event:replace(Key, Value),
    {noreply, State#state{value = Value}, time_left(State)};

handle_cast(delete, State) ->
    {stop, normal, State}.

handle_info(timeout, State) ->
    {stop, normal, State}.

terminate(_Reson, State) ->
    Key = State#state.key,
    sc_event:delete(Key),
    sc_store:delete(Key),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% ==================================================================
%%% Internal functions
%%% ==================================================================

time_left(_StartTime, infinity) ->
    infinity;
time_left(StartTime, LeaseTime) ->
    Now = local_time_as_seconds(),
    TimeElapsed = Now - StartTime,
    Remaining = LeaseTime - TimeElapsed,
    if
        Remaining < 0 -> 0;
        true -> Remaining * 1000
    end.
time_left(#state{lease_time = LeaseTime, start_time = StartTime}) ->
    time_left(StartTime, LeaseTime).

local_time_as_seconds() ->
    Now = calendar:local_time(),
    calendar:datetime_to_gregorian_seconds(Now).
