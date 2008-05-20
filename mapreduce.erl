%%%-------------------------------------------------------------------
%%% File    : mapreduce.erl
%%% Author  : Andreas Hasselberg <andreas.hasselberg@gmail.com>
%%% Description : A non spawning map reduce
%%%
%%% Created :  9 May 2008 by Andreas Hasselberg <andreas.hasselberg@gmail.com>
%%%-------------------------------------------------------------------
-module(mapreduce).


-export([do/3, test/0]).

do(MapF, ReduceF, Data) ->
    do(MapF, ReduceF, Data, infinity).

do(MapF, ReduceF, Data, Timeout) ->
    ReduceData = spawn_map(MapF, Data, Timeout),
    %% ReduceData is a dict with lists of vales
    do_reduce(ReduceF, ReduceData).


%% NON SPAWNING MAP
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

%% SPAWNING MAP
%% DO PROCESS MONITOR INSTEAD OF TIMEOUT IN YIELD.
spawn_map(MapF, DataL, Timeout) ->
    Self = self(),
    Tags = lists:map(fun(Data) ->
			     Tag = make_ref(),
			     spawn(fun() ->
					   Val = MapF(Data),
					   Self ! {Tag, Val}
				   end),
			     Tag
		     end, DataL),
    collect(yield(Tags, Timeout)).

collect(ResDL) ->
    lists:foldl(
      fun(ResL, DictAcc) ->
	      lists:foldl(
		fun({ResKey, ResVal}, DictAccAcc) ->
			dict:append(ResKey, ResVal, DictAccAcc)
		end,
		DictAcc,
		ResL)
      end,
      dict:new(),
      ResDL).

yield([], _Timeout) -> [];
yield([H|T], Timeout) ->
    Val1 =
	receive
	    {H, Val} -> Val
	after Timeout ->
		%% HANDLE ERROR
		[]
	
	end,
    [Val1|yield(T, Timeout)].


%% NON SPAWNING REDUCE
do_reduce(ReduceF, ReduceData) ->
    dict:fold(
      fun(Key, Values, ResL) ->
	      Value = ReduceF(Key, Values),
	      [{Key, Value}|ResL]
      end,
      [],
      ReduceData).

test() ->
    Data = 
	do(fun({Key, Value}) -> [{Key, Value},{1, Value}] end,
	   fun(_Key, Values) -> Values end,
	   [{1, 2}, {3, 4}],
	   1000),
    [{3,[4]},{1,[2,2,4]}] = Data,
    [{3,[4]},{1,[8]}] =
	do(fun({Key, Value}) -> [{Key, lists:sum(Value)}] end,
	   fun(_Key, Values) -> Values end,
	   Data,
	   1000).

    
