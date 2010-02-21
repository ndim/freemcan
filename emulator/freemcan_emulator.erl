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
      || N <- lists:seq(1, ElementCount) ].


checksum(Bin) when is_binary(Bin) ->
    lists:foldl(fun(C, Acc) ->
			N = C,
			X = (8*N+2*N+N) band 16#ffff,
			R = ((Acc bsl 3) band 16#ffff) bor ((Acc bsr 13) band 16#ffff),
			V = R bxor X,
			V
		end,
		16#3e59,
		binary_to_list(Bin)).


frame(text, Text) ->
    bin_frame($T, Text);
frame(status, StatusMsg) ->
    bin_frame($S, StatusMsg);
frame(histogram, Payload) ->
    bin_frame($H, Payload).

bin_frame(Type, Payload) when is_integer(Type)  ->
    BinPayload = list_to_binary(Payload),
    ByteSize = byte_size(BinPayload),
    FrameData = <<"FMPK", ByteSize:16/little-integer, Type, BinPayload/binary>>,
    DummyChecksum = checksum(FrameData),
    list_to_binary([FrameData, <<DummyChecksum>>]).


status_packet(StatusMessage) ->
    frame(status, StatusMessage).


text_packet(StatusMessage) ->
    frame(status, StatusMessage).


histogram_packet(done, Histogram) ->
    histogram_packet($D, Histogram);
histogram_packet(intermediate, Histogram) ->
    histogram_packet($I, Histogram);
histogram_packet(aborted, Histogram) ->
    histogram_packet($A, Histogram);
histogram_packet(Type, Histogram) when is_integer(Type) ->
    Payload = [<<4>>,
	       [ <<Val:32/little-integer>> || Val <- Histogram ]],
    frame(histogram, Payload).


fsm(boot, <<"a">>) ->
    {boot, none, 1};
fsm(boot, <<"i">>) ->
    {boot, none, 1};
fsm(boot, <<"m">>) ->
    {boot, none, 1};
fsm(boot, <<"r">>) ->
    {boot, none, 1};
fsm(boot, {timeout, _}) ->
    {ready, status_packet("READY"), none};

fsm(ready, <<"a">>) ->
    {ready, text_packet("IGNORED"), none};
fsm(ready, <<"i">>) ->
    {ready, text_packet("IGNORED"), none};
fsm(ready, <<"m">>) ->
    {{measuring, 0}, status_packet("Measuring"), 10000};
fsm(ready, <<"r">>) ->
    {reset, none, 100};

fsm({measuring, _}=State, <<"a">>) ->
    {State, histogram_packet(aborted, dummy_histogram()), 10};
fsm({measuring, _}=State, <<"i">>) ->
    {State, histogram_packet(intermediate, dummy_histogram()), 10};
fsm({measuring, _}=State, <<"m">>) ->
    {State, status_packet("IGNORED"), 10};
fsm({measuring, _}=State, <<"r">>) ->
    {State, status_packet("IGNORED"), 10};
fsm({measuring, N}, {timeout, _}) when is_integer(N), N =< 3 ->
    {{measuring, N+1}, histogram_packet(intermediate, dummy_histogram()), 10000};
fsm({measuring, _}, {timeout, _}) ->
    {reset, histogram_packet(done, dummy_histogram()), 0};

fsm(reset, <<"a">>) ->
    {reset, none, 1};
fsm(reset, <<"i">>) ->
    {reset, none, 1};
fsm(reset, <<"m">>) ->
    {reset, none, 1};
fsm(reset, <<"r">>) ->
    {reset, none, 1};
fsm(reset, {timeout, _}) ->
    {boot, status_packet("Resetting"), 100}.


loop(LoopState = #state{port=Port, state=CurState, timeout=TimeOut}) ->
    RealTimeOut = case TimeOut of
		      none -> 100000;
		      N when is_integer(N) -> N
		  end,
    io:format("Current state:    ~p~n", [CurState]),
    receive
	{Port, {data, Cmd}} ->
	    io:format("Port info:        ~p~n", [erlang:port_info(Port)]),
	    io:format("Received command: ~p~n", [Cmd]),
	    {NextState, Reply, NextTimeOut} = fsm(CurState, Cmd),
	    io:format("Sending reply:    ~p~n", [Reply]),
	    Port ! {self(), {command, Reply}},
	    io:format("Next state:       ~p~n", [NextState]),
	    loop(LoopState#state{state=NextState, timeout=NextTimeOut});
	Unhandled ->
	    io:format("Port info:        ~p~n", [erlang:port_info(Port)]),
	    io:format("Unhandled:        ~p~n", [Unhandled]),
	    {error, {unhandled, Unhandled}}
    after RealTimeOut ->
	    case TimeOut of
		none -> loop(LoopState);
		TimeOut ->
		    io:format("Timeout:          ~p~n", [TimeOut]),
		    {NextState, Reply, NextTimeOut} = fsm(CurState, {timeout, TimeOut}),
		    io:format("Sending message:  ~p~n", [Reply]),
		    case Reply of
			none -> ok;
			Reply ->
			    Port ! {self(), {command, Reply}}
		    end,
		    io:format("Next state:       ~p~n", [NextState]),
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
    % spawn(?MODULE, loop, [InitState]),
    loop(InitState).

start() ->
    start([]).

start([FIFO]) when is_atom(FIFO) ->
    intermain(atom_to_list(FIFO));
start([FIFO]) when is_list(FIFO) ->
    intermain(FIFO).
