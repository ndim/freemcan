%%%-------------------------------------------------------------------
%%% @author ndim <hun@n-dimensional.de>
%%% @copyright (C) 2015, ndim
%%% @doc
%%%
%%% @end
%%% Created : 15 Sep 2015 by ndim <hun@n-dimensional.de>
%%%-------------------------------------------------------------------
-module(fmemu_unix_port).

-behaviour(gen_server).

%% API
-export([start_link/1]).
-export([send_state/1]).
-export([send_personality_info/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {port}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(FIFOName) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [FIFOName], []).


send_state(State) ->
    gen_server:cast(?SERVER, {send_state, State}).


send_personality_info() ->
    Data = <<3072:16/little-integer, % table size in bytes
	     24, % table element size in bits
	     1, % units per second
	     2, % param size timer1 count
	     0  % param size skip samples
	   >>,
    Name = <<"emulator">>,
    Personality = [Data, Name],
    gen_server:cast(?SERVER, {send_personality_info, Personality}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([FIFOName]) ->
    ExecName = "erl_unix_port",
    ExecPath = case code:priv_dir(fmemu) of
		   {error, bad_name} ->
		       case code:which(?MODULE) of
			   Atom when is_atom(Atom) ->
			       case file:get_cwd() of
				   {ok, Dir} ->
				       filename:join([Dir, priv, ExecName])
			       end;
			   FName ->
			       filename:join([filename:dirname(FName),
					      '..', 'priv', ExecName])
		       end;
		   PrivPath -> filename:join([PrivPath, ExecName])
	       end,
    {ok, _} = file:open(ExecPath, [read, binary]),
    {Executable, Arg0, Args} =
	case true of
	    false ->
		{ExecPath,
		 ExecName,
		 [FIFOName]};
	    true ->
		{"/usr/bin/strace",
		 "strace",
		 ["-s1024",
		  "-oerl_unix_port.strace",
		  ExecPath,
		  FIFOName]}
	end,
    Port = open_port({spawn_executable, Executable},
		     [nouse_stdio,
		      binary, stream,
		      exit_status,
		      {arg0, Arg0},
		      {args, Args}]),
    {ok, #state{port=Port}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({send_personality_info, PI}, State = #state{port=Port}) ->
    send_frame(Port, $P, PI),
    {noreply, State};
handle_cast({send_state, StateMsg}, State = #state{port=Port}) ->
    send_frame(Port, $S, StateMsg),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({_Port, {data, Bin}}, State) when is_binary(Bin) ->
    %% io:format("~s:handle_info ~p ~p~n", [?MODULE, Bin, State]),
    handle_bytes(Bin),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


handle_bytes(<<Byte, Rest/binary>>) ->
    fmemu_byte_fsm:handle_byte(Byte),
    handle_bytes(Rest);
handle_bytes(<<>>) ->
    ok.


send_frame(Port, Type, Payload) when is_atom(Payload) ->
    send_frame(Port, Type, atom_to_list(Payload));
send_frame(Port, Type, Payload) when is_list(Payload) ->
    send_frame(Port, Type, list_to_binary(Payload));
send_frame(Port, Type, Payload) when is_binary(Payload) ->
    ByteSize = byte_size(Payload),
    FrameData = <<"FMPX", Type, ByteSize:16/little-integer, Payload/binary>>,
    FCS = fmemu_util:checksum(FrameData),
    Frame = <<FrameData/binary, FCS:16/little-integer>>,
    io:format("sending frame ~p ~w~n", [Frame, fmemu_util:checksum(Frame)]),
    Port ! {self(), {command, Frame}}.
