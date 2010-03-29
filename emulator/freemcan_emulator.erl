%%% \file emulator/freemcan_emulator.erl
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

-export([checksum/1]).

-define(DEFAULT_TIMEOUT, 5000).

-record(state, {port, state=boot, timeout=100}).


checksum(Bin) when is_binary(Bin) ->
    State =lists:foldl(fun(C, Acc) ->
			       N = C,
			       X = (8*N+2*N+N) band 16#ffff,
			       R = ((Acc bsl 3) band 16#ffff) bor ((Acc bsr 13) band 16#ffff),
			       V = (R bxor X) band 16#ffff,
			       V
		       end,
		       16#3e59,
		       binary_to_list(Bin)),
    State band 16#ff.


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
    Checksum = checksum(FrameData),
    list_to_binary([FrameData, <<Checksum>>]).


status_packet(StatusMessage) ->
    frame(status, StatusMessage).


text_packet(StatusMessage) ->
    frame(status, StatusMessage).


histogram_packet(done, Seconds) when is_integer(Seconds) ->
    histogram_packet($D, Seconds, histogram_emulator:histogram(Seconds));
histogram_packet(intermediate, Seconds) when is_integer(Seconds) ->
    histogram_packet($I, Seconds, histogram_emulator:histogram(Seconds));
histogram_packet(aborted, Seconds) when is_integer(Seconds) ->
    histogram_packet($A, Seconds, histogram_emulator:histogram(Seconds)).

histogram_packet(Type, Seconds, Histogram) when is_integer(Type) ->
    Payload = [<<3>>, Type, <<Seconds:16/little-integer>>,
	       [ <<Val:24/little-integer>> || Val <- Histogram ]],
    frame(histogram, Payload).


decode_cmd(<<"a">>) ->
    abort;
decode_cmd(<<"r">>) ->
    reset;
decode_cmd(<<"i">>) ->
    intermediate;
decode_cmd(<<"m", TimerBin:2/binary, Checksum>>) ->
    <<Periods:16/little-integer>> = TimerBin,
    Data = list_to_binary([<<"m">>, TimerBin]),
    OwnChecksum = checksum(Data),
    Checksum = OwnChecksum,
    {measure, Periods};
decode_cmd(Binary) ->
    Binary.


fsm(boot, {timeout, _}) ->
    {ready, status_packet("READY"), none};

fsm(ready, {measure, Seconds}) ->
    {{measuring, 0, Seconds}, status_packet("Measuring"), 1000};
fsm(ready, reset) ->
    {reset, none, 100};
fsm(ready, abort) ->
    {ready, status_packet("READY"), none};
fsm(ready, intermediate) ->
    {ready, status_packet("READY"), none};

fsm({measuring, SecondsDone, _SecondsTotal}, abort) ->
    {reset, histogram_packet(aborted, SecondsDone), 10};
fsm({measuring, SecondsDone, _SecondsTotal}=State, intermediate) ->
    %% FIXME: Use the proper timeout here :-/
    {State, histogram_packet(intermediate, SecondsDone), 1000};
fsm({measuring, SecondsDone=SecondsTotal, SecondsTotal}, {timeout, _}) ->
    {reset, histogram_packet(done, SecondsDone), 0};
fsm({measuring, SecondsDone, SecondsTotal}, {timeout, Period}) ->
    {{measuring, SecondsDone+1, SecondsTotal}, text_packet("STILL MEASURING"), Period};

fsm(reset, {timeout, _}) ->
    {boot, status_packet("Resetting"), 100}.


send_reply(_Port, none) ->
    none;
send_reply(Port, Reply) when is_binary(Reply) ->
    Size = size(Reply),
    send_reply(Port, Size, Reply).

send_reply(Port, Size, Reply) when Size > 2 ->
    Size1 = random:uniform(Size),
    {Reply1, Reply2} = erlang:split_binary(Reply, Size1),
    send_reply_int(Port, Reply1),
    send_reply_int(Port, Reply2);
send_reply(Port, _Size, Reply) ->
    send_reply_int(Port, Reply).

send_reply_int(Port, Reply) when size(Reply) > 0 ->
    Port ! {self(), {command, Reply}};
send_reply_int(_, _) ->
    ok.


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
	    {NextState, Reply, NextTimeOut} = fsm(CurState, decode_cmd(Cmd)),
	    io:format("Sending reply:    ~P~n", [Reply,30]),
	    send_reply(Port, Reply),
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
		    io:format("Sending message:  ~P~n", [Reply,30]),
		    send_reply(Port, Reply),
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
