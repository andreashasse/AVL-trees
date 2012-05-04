%%%-------------------------------------------------------------------
%%% File    : bench.erl
%%% Author  : Andreas Hasselberg <andreas.hasselberg@gmail.com>
%%% Description : Mojselimojs
%%%
%%% Created : 15 Jan 2008 by Andreas Hasselberg <>
%%%-------------------------------------------------------------------
-module(avl_bench).

-compile([export_all]).

bench() ->
    RandTimes = do_bench(100000, rand),
    SortTimes = do_bench(100000, sort),
    io:format("rand ~p", [RandTimes]),
    io:format("sort ~p", [SortTimes]).

clock__time() ->
    {element(2, statistics(runtime)), element(2, statistics(wall_clock))}.

do_bench(N, R) ->
    Keys = case R of
	       rand -> Rand = [ {random:uniform(N),X} || X <- lists:seq(1, N) ],
		       [ X || {_,X} <- lists:sort(Rand)];
	       sort -> lists:seq(1, N)
	   end,
    Values = lists:seq(1, N*2, 2),
    List = lists:zip(Keys, Values),
    AvlTimes = do_avl(List),
    GbTimes = do_gb(List),
    {AvlTimes, GbTimes}.

do_avl(List) ->
    %% insert
    {_, _} = clock__time(),
    Insert = fun({Key, Val}, TreeAcc) -> avl:insert(Key, Val, TreeAcc) end,
    Tree = lists:foldl(Insert, avl:empty(), List),
    {Rtime1, _Wall1} = clock__time(),
    %% lookup
    Lookup = fun({Key, Val}) -> {value, Val} == avl:lookup(Key, Tree) end,
    true = lists:all(Lookup, List),
    {Rtime2, _Wall2} = clock__time(),
    %% delete
    Delete = fun({Key, _Val}, TreeAcc) -> avl:delete(Key, TreeAcc) end,
    lists:foldl(Delete, Tree, List),
    {Rtime3, _Wall3} = clock__time(),
    {Rtime1, Rtime2, Rtime3}.

do_gb(List) ->
    %% insert
    {_, _} = clock__time(),
    Insert = fun({Key, Val}, TreeAcc) -> gb_trees:insert(Key, Val, TreeAcc) end,
    Tree = lists:foldl(Insert, gb_trees:empty(), List),
    {Rtime1, _Wall1} = clock__time(),
    %% lookup
    Lookup = fun({Key, Val}) -> {value, Val} == gb_trees:lookup(Key, Tree) end,
    true = lists:all(Lookup, List),
    {Rtime2, _Wall2} = clock__time(),
    %% delete
    Delete = fun({Key, _Val}, TreeAcc) -> gb_trees:delete(Key, TreeAcc) end,
    lists:foldl(Delete, Tree, List),
    {Rtime3, _Wall3} = clock__time(),
    {Rtime1, Rtime2, Rtime3}.

