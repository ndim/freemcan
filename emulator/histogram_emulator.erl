-module(histogram_emulator).

-export([histogram/1]).

gauss(Sigma, X) ->
    Frac = X / Sigma,
    F2 = Frac * Frac,
    Scale = 1 / (Sigma * math:sqrt(2*math:pi())),
    Scale * math:exp(-0.5*F2).

histogram(Seconds) ->
    ElementCount = 1024,
    Peaks = [{60, 1, 25}, {150,1.5,30}, {220, 0.6, 20}],
    [ round(Seconds*100.0*lists:sum([ F*gauss(S, (N-P))
				    || {P,F,S} <- Peaks ]) +
	    2.0*random:uniform())
      || N <- lists:seq(1,ElementCount)].

