%%%-------------------------------------------------------------------
%%% @author ndim <hun@n-dimensional.de>
%%% @copyright (C) 2015, ndim
%%% @doc
%%%
%%% @end
%%% Created : 16 Sep 2015 by ndim <hun@n-dimensional.de>
%%%-------------------------------------------------------------------
-module(fmemu_util).

%% API
-export([checksum/1]).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
checksum(Bin) when is_binary(Bin) ->
    lists:foldl(fun(D0, CRC) ->
			Lo8 = (CRC bsr 0)         band 16#00ff,
			Hi8 = (CRC bsr 8)         band 16#00ff,
			D1 = (D0 bxor Lo8)        band 16#00ff,
			D2 = (D1 bxor (D1 bsl 4)) band 16#00ff,
			A = ((D2 bsl 8) bor Hi8)  band 16#ffff,
			B = (D2 bsr 4)            band 16#00ff,
			C = (D2 bsl 3)            band 16#ffff,
			(A bxor B bxor C)         band 16#ffff
		end,
		16#ffff,
		binary_to_list(Bin)).


%%%===================================================================
%%% Internal functions
%%%===================================================================
