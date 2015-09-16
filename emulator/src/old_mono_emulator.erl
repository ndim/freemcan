%%% \file emulator/fmemu_util.erl
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

-module(fmemu_util).

-export([start/0, start/1]).

-export([frame_loop/1]).
-export([byte_loop/1]).
-export([loop/2]).

-define(DEFAULT_TIMEOUT, 5000).


frame(text, Text) ->
    bin_frame($T, Text);
frame(state, StateMsg) ->
    bin_frame($S, StateMsg);
frame(histogram, Payload) ->
    bin_frame($V, Payload).


bin_frame(Type, Payload) when is_integer(Type)  ->
    BinPayload = list_to_binary(Payload),
    ByteSize = byte_size(BinPayload),
    FrameData = <<"FMPX", ByteSize:16/little-integer, Type, BinPayload/binary>>,
    Checksum = checksum(FrameData),
    list_to_binary([FrameData, <<Checksum:16/little-integer>>]).


state_packet(State) when is_atom(State) ->
    frame(state, string:to_upper(atom_to_list(State)));
state_packet(State) when is_list(State) ->
    frame(state, State).


text_packet(StatusMessage) ->
    frame(text, StatusMessage).


parse_frame(<<"FMPX", Cmd, Len, Parms:Len/binary, FCS:16>>) ->
    case (FCS =:= checksum(<<"FMPX", Cmd, Len, Parms/binary>>)) of
	true when Len > 0 ->
	    {cmd, Cmd, Len, Parms};
	true ->
	    {cmd, Cmd};
	false ->
	    {error, chksum}
    end.


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
    {ready, state_packet("READY"), none};

fsm(ready, {measure, Seconds}) ->
    {{measuring, 0, Seconds}, state_packet("Measuring"), 1000};
fsm(ready, reset) ->
    {reset, none, 100};
fsm(ready, abort) ->
    {ready, state_packet("READY"), none};
fsm(ready, intermediate) ->
    {ready, state_packet("READY"), none};

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
    {boot, state_packet("Resetting"), 100}.


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


personality_info() ->
    Data = <<3072:16/little-integer, % table size in bytes
	     24, % table element size in bits
	     1, % units per second
	     0, % param size timer1 count
	     0  % param size skip samples
	   >>,
    Name = <<"emulator">>,
    [Data, Name].


frame_loop(CmdPort) ->
    send_reply(CmdPort, state_packet("booting")),
    frame_loop(CmdPort, ready).

frame_loop(CmdPort, State) ->
    {NextState, Reply} = receive
			     {cmd, $f} ->
				 {State, bin_frame($P, personality_info())};
			     {cmd, $s} ->
				 {State, state_packet(State)}
			 end,
    io:format("Sending reply:     ~P~n", [Reply,30]),
    send_reply(CmdPort, Reply),
    io:format("Next state:        ~p~n", [NextState]),
    frame_loop(CmdPort, NextState).


byte_loop(FrameLoop) ->
    byte_loop(FrameLoop, {stf_magic, 0}, <<>>).

byte_loop(FrameLoop, State = {StateName, StateParm}, Acc) ->
    {NextState, NextAcc} = receive
			       {byte, $F} when State == {stf_magic, 0} ->
				   {{stf_magic, 1}, <<Acc/binary, "F">>};
			       {byte, $M} when State == {stf_magic, 1} ->
				   {{stf_magic, 2}, <<Acc/binary, "M">>};
			       {byte, $P} when State == {stf_magic, 2} ->
				   {{stf_magic, 3}, <<Acc/binary, "P">>};
			       {byte, $X} when State == {stf_magic, 3} ->
				   {{stf_cmd, none}, <<Acc/binary, "X">>};
			       {byte, Cmd} when State == {stf_cmd, none} ->
				   {{stf_len, none}, <<Acc/binary, Cmd>>};
			       {byte, Len} when State == {stf_len, none} ->
				   {{stf_params, Len}, <<Acc/binary, Len>>};
			       {byte, ParmByte} when State == {stf_params, 0} ->
				   {{stf_checksum, 1}, <<Acc/binary, ParmByte>>};
			       {byte, ParmByte} when StateName == stf_params,
						     StateParm > 0 ->
				   {{stf_params, StateParm-1}, <<Acc/binary, ParmByte>>};
			       {byte, FCSByte} when State == {stf_checksum, 0} ->
				   {{stf_checksum, 1}, <<Acc/binary, FCSByte>>};
			       {byte, FCSByte} when State == {stf_checksum, 1} ->
				   BinFrame = <<Acc/binary, FCSByte>>,
				   Frame = parse_frame(BinFrame),
				   case Frame of
				       {cmd, _Cmd, _Len, _Parms} = C ->
					   io:format("Frame: ~p ~p~n", [Frame, C]),
					   FrameLoop ! C;
				       {cmd, _Cmd} = C ->
					   io:format("Frame: ~p ~p~n", [Frame, C]),
					   FrameLoop ! C
				   end,
				   {{stf_magic, 0}, <<>>};
			       {byte, Byte} ->
				   io:format("Ignoring byte: ~p~n", [<<Byte>>]),
				   {{stf_magic, 0}, <<>>}
			   end,
    byte_loop(FrameLoop, NextState, NextAcc).


loop(FIFOPort, ByteLoop) ->
    receive
	{Port, {data, Data}} ->
	    io:format("Port info:         ~p~n", [erlang:port_info(Port)]),
	    io:format("Received cmd data: ~p~n", [Data]),
	    [ ByteLoop ! {byte, Byte} || <<Byte>> <= Data ],
	    loop(FIFOPort, ByteLoop);
	Unhandled ->
	    io:format("Port info:        ~p~n", [erlang:port_info(FIFOPort)]),
	    io:format("Unhandled:        ~p~n", [Unhandled]),
	    {error, {unhandled, Unhandled}}
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
    Port.


intermain(FIFO) ->
    FIFOPort = init(FIFO),
    % spawn(?MODULE, loop, [InitState]),
    FrameLoop = spawn(?MODULE, frame_loop, [FIFOPort]),
    ByteLoop = spawn(?MODULE, byte_loop, [FrameLoop]),
    loop(FIFOPort, ByteLoop).

start() ->
    start([]).

start([FIFO]) when is_atom(FIFO) ->
    intermain(atom_to_list(FIFO));
start([FIFO]) when is_list(FIFO) ->
    intermain(FIFO).
