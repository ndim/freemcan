%%%-------------------------------------------------------------------
%%% @author ndim <hun@n-dimensional.de>
%%% @copyright (C) 2015, ndim
%%% @doc
%%%
%%% @end
%%% Created : 15 Sep 2015 by ndim <hun@n-dimensional.de>
%%%-------------------------------------------------------------------
-module(fmemu_frame_fsm).

-behaviour(gen_fsm).

%% API
-export([start_link/0]).
-export([handle_frame_bytes/1]).

%% gen_fsm callbacks
-export([init/1, handle_event/3,
	 handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-export([booting/2]).
-export([stp_ready/2]).
-export([stp_measuring/2]).
-export([stp_done/2]).
-export([reset/2]).

-define(SERVER, ?MODULE).

-record(state, {ts}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_fsm:start_link({local, ?SERVER}, ?MODULE, [], []).


handle_frame_bytes(FrameBytes =
		       <<"FMPX", Cmd, Len, Params:Len/binary,
			 _FCS:16/little-integer>>) ->
    CS = fmemu_util:checksum(FrameBytes),
    io:format("FRAME BYTES ~p ~.16#~n", [FrameBytes, CS]),
    case CS of
	0  -> case Len of
		  0   -> gen_fsm:send_event(?SERVER, {cmd, Cmd});
		  Len -> gen_fsm:send_event(?SERVER, {cmd, Cmd, Params})
	      end;
	CS -> checksum_fail
    end.


%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @spec init(Args) -> {ok, StateName, State} |
%%                     {ok, StateName, State, Timeout} |
%%                     ignore |
%%                     {stop, StopReason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, booting, #state{}, 1000}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_event/2, the instance of this function with the same
%% name as the current state name StateName is called to handle
%% the event. It is also called if a timeout occurs.
%%
%% @spec state_name(Event, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
%state_name(_Event, State) ->
%    {next_state, state_name, State}.

booting(timeout, State) ->
    io:format("Finished booting~n", []),
    fmemu_unix_port:send_state(booting),
    ssr({next_state, stp_ready, State}).

stp_ready({cmd, $f}, State) ->
    fmemu_unix_port:send_personality_info(),
    nsr({next_state, stp_ready, State});
stp_ready({cmd, $s}, State) ->
    fmemu_unix_port:send_state(stp_ready),
    nsr({next_state, stp_ready, State});
stp_ready({cmd, $r}, State) ->
    ssr({next_state, reset, State, 1000});
stp_ready({cmd, $m}, State) ->
    ssr({next_state, stp_measuring, State});
stp_ready({cmd, $m, <<TimerCount:16/little-integer, Timestamp:64/little-integer>>}, State) ->
    ssr({next_state, stp_measuring, State#state{ts=Timestamp}, TimerCount*1000});
stp_ready({cmd, UnhandledCmd}, State) ->
    io:format("Unhandled command '~c'=~w~n",
	      [UnhandledCmd, UnhandledCmd]),
    nsr({next_state, stp_ready, State}).

stp_measuring(timeout, State) ->
    %% TODO: Send 'D' 'measured' data
    ssr({next_state, stp_done, State});
stp_measuring({cmd, $a}, State) ->
    %% TODO: Send 'A' 'measured' data
    ssr({next_state, stp_done, State});
stp_measuring({cmd, $f}, State) ->
    fmemu_unix_port:send_personality_info(),
    nsr({next_state, stp_measuring, State});
stp_measuring({cmd, $s}, State) ->
    fmemu_unix_port:send_state(stp_measuring),
    nsr({next_state, stp_measuring, State});
stp_measuring({cmd, _}, State) ->
    nsr({next_state, stp_measuring, State}).

stp_done({cmd, $f}, State) ->
    fmemu_unix_port:send_personality_info(),
    ssr({next_state, stp_done, State});
stp_done({cmd, $s}, State) ->
    ssr({next_state, stp_done, State});
stp_done({cmd, $r}, State) ->
    ssr({next_state, reset, State});
stp_done({cmd, _}, State) ->
    %% TODO: Resend 'R' data table
    ssr({next_state, stp_done, State});
stp_done({cmd, _, _}, State) ->
    %% TODO: Resend 'R' data table
    ssr({next_state, stp_done, State}).

reset(timeout, _State) ->
    fmemu_unix_port:send_state(reset),
    ssr({next_state, booting, #state{}, 1000}).


nsr(R) ->
    R.

ssr(R = {next_state, StateName, _State}) ->
    fmemu_unix_port:send_state(StateName),
    R;
ssr(R = {next_state, StateName, _State, _Timeout}) ->
    fmemu_unix_port:send_state(StateName),
    R.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_event/[2,3], the instance of this function with
%% the same name as the current state name StateName is called to
%% handle the event.
%%
%% @spec state_name(Event, From, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
%state_name(_Event, _From, State) ->
%    Reply = ok,
%    {reply, Reply, state_name, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%
%% @spec handle_event(Event, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/[2,3], this function is called
%% to handle the event.
%%
%% @spec handle_sync_event(Event, From, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it receives any
%% message other than a synchronous or asynchronous event
%% (or a system message).
%%
%% @spec handle_info(Info,StateName,State)->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%
%% @spec terminate(Reason, StateName, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, StateName, State, Extra) ->
%%                   {ok, StateName, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
