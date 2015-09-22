-module(fmemu_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    fmemu_sup:start_link([foobar]).

stop(_State) ->
    ok.
