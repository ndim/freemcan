-module(histogram_emulator).

-export([histogram/1]).

gauss(Sigma, X) ->
    Frac = X / Sigma,
    F2 = Frac * Frac,
    Scale = 1 / (Sigma * math:sqrt(2*math:pi())),
    Scale * math:exp(-0.5*F2).

histogram(Seconds) ->
    ElementCount = 512,
    Sigma = 25,
    Peaks = [60, 150],
    [ round(Seconds*170*lists:sum([ gauss(Sigma, (N-P)) || P <- Peaks ]))
      || N <- lists:seq(1,ElementCount)].

