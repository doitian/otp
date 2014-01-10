-module(sc_element_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_child/3]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Restart, Shutdown, Type), {I, {I, start_link, []}, Restart, Shutdown, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(Key, Value, LeaseTime) ->
    supervisor:start_child(?MODULE, [Key, Value, LeaseTime]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    Element = ?CHILD(sc_element, temporary, brutal_kill, worker),
    RestartStrategy = {simple_one_for_one, 0, 1},
    {ok, {RestartStrategy, [Element]}}.
