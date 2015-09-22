%%%-------------------------------------------------------------------
%%% @author ndim <hun@n-dimensional.de>
%%% @copyright (C) 2015, ndim
%%% @doc
%%%
%%% @end
%%% Created : 15 Sep 2015 by ndim <hun@n-dimensional.de>
%%%-------------------------------------------------------------------
-module(fmemu_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link([Args]) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [Args]).


%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([FIFOName]) ->
    RestartStrategy = one_for_all,
    MaxRestarts = 3,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    UChild = {fmemu_unix_port, {fmemu_unix_port, start_link, [FIFOName]},
	      Restart, Shutdown, Type, [fmemu_unix_port]},
    BChild = {fmemu_byte_fsm, {fmemu_byte_fsm, start_link, []},
	      Restart, Shutdown, Type, [fmemu_byte_fsm]},
    FChild = {fmemu_frame_fsm, {fmemu_frame_fsm, start_link, []},
	      Restart, Shutdown, Type, [fmemu_frame_fsm]},

    {ok, {SupFlags, [BChild, FChild, UChild]}}.


%%%===================================================================
%%% Internal functions
%%%===================================================================
