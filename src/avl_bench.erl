%%%-------------------------------------------------------------------
%%% File    : bench.erl
%%% Author  : Andreas Hasselberg <andreas.hasselberg@gmail.com>
%%% Description : Mini benchmark to see if avl.erl is anywhere nere gb_trees.erl
%%%
%%% Created : 15 Jan 2008 by Andreas Hasselberg <>
%%%-------------------------------------------------------------------
-module(avl_bench).

-export([bench/0]).

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
    do_tree(avl, List).

do_gb(List) ->
    do_tree(gb_trees, List).

do_tree(Mod, List) ->
    %% insert
    {_, _} = clock__time(),
    Insert = fun({Key, Val}, TreeAcc) -> Mod:insert(Key, Val, TreeAcc) end,
    Tree = lists:foldl(Insert, Mod:empty(), List),
    {Rtime1, _Wall1} = clock__time(),
    %% lookup
    Lookup = fun({Key, Val}) -> {value, Val} == Mod:lookup(Key, Tree) end,
    true = lists:all(Lookup, List),
    {Rtime2, _Wall2} = clock__time(),
    %% delete
    Delete = fun({Key, _Val}, TreeAcc) -> Mod:delete(Key, TreeAcc) end,
    lists:foldl(Delete, Tree, List),
    {Rtime3, _Wall3} = clock__time(),
    {Rtime1, Rtime2, Rtime3}.
