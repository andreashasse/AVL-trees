%%%-------------------------------------------------------------------
%%% File    : mapreduce.erl
%%% Author  : Andreas Hasselberg <andreas.hasselberg@gmail.com>
%%% Description : 
%%%
%%% Created :  9 May 2008 by Andreas Hasselberg <andreas.hasselberg@gmail.com>
%%%-------------------------------------------------------------------
-module(mapreduce).


-export([do/3]).

%% mapreduce:do(
%%   fun({Key, Value}) -> [{Key, Value},{1, Values}] end,
%%   fun(Key, Values) -> Values end,
%%   [{1, 2}, {3, 4}]).

do(MapF, ReduceF, Data) ->
    ReduceData = do_map(MapF, Data),
    %% ReduceData is a dict with lists of vales
    do_reduce(ReduceF, ReduceData).


do_map(MapF, DataL) ->
    lists:foldl(
      fun(Data, DictAcc) ->
	      Res = MapF(Data),
	      lists:foldl(
		fun({ResKey, ResVal}, DictAccAcc) ->
			dict:append(ResKey, ResVal, DictAccAcc)
		end,
		DictAcc,
		Res)
      end,
      dict:new(),
      DataL).

do_reduce(ReduceF, ReduceData) ->
    dict:fold(
      fun(Key, Values, ResL) ->
	      Value = ReduceF(Key, Values),
	      [{Key, Value}|ResL]
      end,
      [],
      ReduceData).
