-module(freemcan_emulator).

-export([start/0, start/1]).

-export([loop/1]).

-record(state, {port}).

loop(State = #state{port=Port}) ->
    receive
	{Port, {data, CmdBin}} ->
	    io:format("Received command: ~p~n", [CmdBin]),
	    Reply = <<"Moo">>,
	    io:format("Sending reply:    ~p~n", [Reply]),
	    Port ! Reply,
	    loop(State);
	{'EXIT', Port, Reason} ->
	    io:format("EXIT on Port:     ~p~n", [Reason]),
	    {error, Reason};
	Unhandled ->
	    io:format("Unhandled:        ~p~n", [Unhandled]),
	    loop(State)
    end.

main(FIFO) ->
    io:format("FIFO=~s~n", [FIFO]),
    {ok, Cwd} = file:get_cwd(),
    ExecName = "erl_unix_port",
    Executable = filename:join([Cwd, ExecName]),
    Port = open_port({spawn_executable, Executable},
		     [nouse_stdio, binary, stream,
		      {arg0, ExecName},
		      {args, [FIFO]}
		     ]),
    io:format("Port: ~p~n", [Port]),
    loop(#state{port=Port}).

start() ->
    start([]).

start([FIFO]) when is_atom(FIFO) ->
    main(atom_to_list(FIFO));
start([FIFO]) when is_list(FIFO) ->
    main(FIFO).
