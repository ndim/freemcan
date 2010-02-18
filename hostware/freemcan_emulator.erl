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

fsm(boot, {timeout, _}) ->
    {start, <<"Booted.\n">>, none};

fsm(start, <<"r">>) ->
    {reset, none, 100};
fsm(start, <<"m">>) ->
    {{measuring, 0}, <<"Started measurement\n">>, 10000};

fsm({measuring, N}, {timeout, _}) when is_integer(N), N =< 3 ->
    {{measuring, N+1}, <<"Still measuring\n">>, 10000};
fsm({measuring, _}, {timeout, _}) ->
    {reset, <<"Measurement finished\n">>, 0};

fsm(reset, {timeout, _}) ->
    {boot, <<"Resetting\n">>, 100}.


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
