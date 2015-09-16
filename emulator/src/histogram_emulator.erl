-module(histogram_emulator).

-export([histogram/1]).
-export([to_file/1, to_file/2]).

gauss(Sigma, X) ->
    Frac = X / Sigma,
    F2 = Frac * Frac,
    Scale = 1 / (Sigma * math:sqrt(2*math:pi())),
    Scale * math:exp(-0.5*F2).

histogram(Seconds) ->
    ElementCount = 1024,
    Peaks = [{120, 1, 25}, {300,1.5,30}, {440, 0.6, 20}, {820, 0.4, 30}],
    [ round(Seconds*100.0*lists:sum([ F*gauss(S, (N-P))
				    || {P,F,S} <- Peaks ]) +
	    2.0*random:uniform())
      || N <- lists:seq(1,ElementCount)].


to_file([Seconds, FName]) when is_atom(Seconds), is_atom(FName) ->
    to_file([atom_to_list(Seconds), atom_to_list(FName)]);
to_file([Seconds, FName]) when is_list(Seconds), is_list(FName) ->
    to_file(list_to_integer(Seconds), FName).

to_file(Seconds, FName) ->
    Histogram = histogram(Seconds),
    Data = [ <<Val:24/little-integer>> || Val <- Histogram ],
    file:write_file(FName, Data).
