%%%-------------------------------------------------------------------
%%% @author ndim <hun@n-dimensional.de>
%%% @copyright (C) 2015, ndim
%%% @doc
%%%
%%% @end
%%% Created : 15 Sep 2015 by ndim <hun@n-dimensional.de>
%%%-------------------------------------------------------------------
-module(fmemu_byte_fsm).

-behaviour(gen_fsm).

%% API
-export([start_link/0]).
-export([handle_byte/1]).

%% gen_fsm callbacks
-export([init/1, handle_event/3,
	 handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).
-export([stf_magic/2,
	 stf_cmd/2,
	 stf_len/2,
	 stf_params/2,
	 stf_checksum/2]).

-define(SERVER, ?MODULE).

-record(state, {cmd=0, index=0, len=0, params=none, fcs=none}).

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

handle_byte(Byte) ->
    case Byte of
	Byte when 32 =< Byte, Byte =< 126 ->
	    io:format("  byte '~c' = 0x~2.16.0B = ~3.10B~n",
		      [Byte, Byte, Byte]);
	Byte ->
	    io:format("  byte     = 0x~2.16.0B = ~3.10B~n",
		      [Byte, Byte])
    end,
    gen_fsm:send_event(?SERVER, {byte, Byte}).


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
    {ok, stf_magic, #state{}}.

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

stf_magic({byte, $F}, State = #state{index=0}) ->
    {next_state, stf_magic, State#state{index=1}};
stf_magic({byte, $M}, State = #state{index=1}) ->
    {next_state, stf_magic, State#state{index=2}};
stf_magic({byte, $P}, State = #state{index=2}) ->
    {next_state, stf_magic, State#state{index=3}};
stf_magic({byte, $X}, State = #state{index=3}) ->
    {next_state, stf_cmd, State}.

stf_cmd({byte, Cmd}, State) when $a =< Cmd, Cmd =< $z ->
    {next_state, stf_len, State#state{cmd=Cmd}}.

stf_len({byte, 0}, State) ->
    {next_state, stf_checksum, State#state{index=0}};
stf_len({byte, Len}, State) ->
    {next_state, stf_params, State#state{index=1, len=Len}}.

stf_params({byte, Byte}, State = #state{index=Idx, len=Len, params=P})
  when Idx < Len ->
    NP = case P of
	     none -> <<Byte>>;
	     P -> <<P/binary, Byte>>
	 end,
    {next_state, stf_params, State#state{index=Idx+1, params=NP}};
stf_params({byte, Byte}, State = #state{params=P}) ->
    NP = <<P/binary, Byte>>,
    {next_state, stf_checksum, State#state{index=0, params=NP}};
stf_params({byte, Byte}, State) ->
    io:format("Unhandled event in state stf_params: byte ~w with state ~p~n",
	      [Byte, State]),
    {next_state, stf_magic, #state{}}.

stf_checksum({byte, Byte}, State = #state{index=0}) ->
    {next_state, stf_checksum, State#state{index=1, fcs= <<Byte>>}};
stf_checksum({byte, Byte}, State = #state{index=1, len=Len,
					  cmd=Cmd, params=Params,
					  fcs=OldFCS}) ->
    S = State#state{fcs= <<OldFCS/binary, Byte>>},
    #state{fcs=FCS} = S,
    Frame = case Len of
		0   -> <<"FMPX", Cmd, Len, FCS/binary>>;
		Len -> <<"FMPX", Cmd, Len, Params/binary, FCS/binary>>
	    end,
    io:format("~s finished composing frame ~p~n", [?MODULE, Frame]),
    fmemu_frame_fsm:handle_frame_bytes(Frame),
    {next_state, stf_magic, #state{}}.


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
