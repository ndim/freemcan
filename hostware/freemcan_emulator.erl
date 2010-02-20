%%% \file freemcan_emulator.erl
%%% \brief Emulate freemcan hardware as it talks via the serial interface
%%%
%%% \author Copyright (C) 2010 Hans Ulrich Niedermann <hun@n-dimensional.de>
%%%
%%% This library is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU Lesser General Public License
%%% as published by the Free Software Foundation; either version 2.1
%%% of the License, or (at your option) any later version.
%%%
%%% This library is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% Lesser General Public License for more details.
%%%
%%% You should have received a copy of the GNU Lesser General Public
%%% License along with this library; if not, write to the Free
%%% Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
%%% Boston, MA 02110-1301 USA

-module(freemcan_emulator).

-export([start/0, start/1]).

-export([loop/1]).

-define(DEFAULT_TIMEOUT, 5000).

-record(state, {port, state=boot, timeout=100}).


dummy_histogram() ->
    ElementCount = 256,
    [ (((301*N*N*N)+(37*N*N)) rem (1 bsl 32))
      || N <- lists:seq(ElementCount) ].


checksum(Bin) when is_binary(Bin) ->
    Mod = (1 bsl 16),
    lists:foldl(fun(C, Acc) ->
			N = C,
			X = (8*N+2*N+N) rem Mod,
			R = ((Acc bsl 3) rem Mod) bor ((Acc bsr 13) rem Mod),
			R bxor X
		end,
		16#3e59,
		binary_to_list(Bin)).


frame(text, Text) ->
    frame($T, Text);
frame(status, StatusMsg) ->
    frame($S, StatusMsg);
frame(histogram, Histogram) ->
    BinHist = [ <<Val:32/little-integer>> || Val <- Histogram ],
    Payload = list_to_binary([<<4>>|BinHist]),
    frame($H, Payload);
frame(Type, Payload) when is_integer(Type), is_binary(Payload) ->
    BinPayload = list_to_binary(Payload),
    Length = length(Payload),
    DummyChecksum = checksum(BinPayload),
    <<"FMPK", Length:16/little-integer, Type, Payload/binary, DummyChecksum>>.


fsm(boot, {timeout, _}) ->
    {start, frame(status, "Booted"), none};

fsm(start, <<"r">>) ->
    {reset, none, 100};
fsm(start, <<"m">>) ->
    {{measuring, 0}, frame(status, "Measuring"), 10000};

fsm({measuring, N}, {timeout, _}) when is_integer(N), N =< 3 ->
    {{measuring, N+1}, frame(text, "Still measuring"), 10000};
fsm({measuring, _}, {timeout, _}) ->
    {reset, frame(histogram, dummy_histogram()), 0};

fsm(reset, {timeout, _}) ->
    {boot, frame(status, "Resetting"), 100}.


loop(LoopState = #state{port=Port, state=CurState, timeout=TimeOut}) ->
    RealTimeOut = case TimeOut of
		      none -> 100000;
		      N when is_integer(N) -> N
		  end,
    receive
	{Port, {data, Cmd}} ->
	    io:format("Port info:        ~p~n", [erlang:port_info(Port)]),
	    io:format("Received command: ~p~n", [Cmd]),
	    {NextState, Reply, NextTimeOut} = fsm(CurState, Cmd),
	    io:format("Sending reply:    ~p~n", [Reply]),
	    Port ! {self(), {command, Reply}},
	    loop(LoopState#state{state=NextState, timeout=NextTimeOut});
	Unhandled ->
	    io:format("Port info:        ~p~n", [erlang:port_info(Port)]),
	    io:format("Unhandled:        ~p~n", [Unhandled]),
	    {error, {unhandled, Unhandled}}
    after RealTimeOut ->
	    case TimeOut of
		none -> loop(LoopState);
		TimeOut ->
		    {NextState, Reply, NextTimeOut} = fsm(CurState, {timeout, TimeOut}),
		    io:format("Timeout:          ~p~n", [TimeOut]),
		    io:format("Sending reply:    ~p~n", [Reply]),
		    case Reply of
			none -> ok;
			Reply ->
			    Port ! {self(), {command, Reply}}
		    end,
		    loop(LoopState#state{state=NextState, timeout=NextTimeOut})
	    end
    end.

init(FIFO) ->
    io:format("FIFO=~s~n", [FIFO]),
    {ok, Cwd} = file:get_cwd(),
    ExecName = "erl_unix_port",
    {Executable, Arg0, Args} =
	case true of
	    false ->
		{filename:join([Cwd, ExecName]),
		 ExecName,
		 [FIFO]};
	    true ->
		{"/usr/bin/strace",
		 "strace",
		 ["-s1024",
		  "-oerl_unix_port.strace",
		  filename:join([Cwd, ExecName]),
		  FIFO]}
	end,
    Port = open_port({spawn_executable, Executable},
		     [nouse_stdio, binary, stream,
		      {arg0, Arg0},
		      {args, Args}
		     ]),
    io:format("Port: ~p~n", [Port]),
    #state{port=Port}.

intermain(FIFO) ->
    InitState = init(FIFO),
    spawn(?MODULE, loop, [InitState]).

start() ->
    start([]).

start([FIFO]) when is_atom(FIFO) ->
    intermain(atom_to_list(FIFO));
start([FIFO]) when is_list(FIFO) ->
    intermain(FIFO).
