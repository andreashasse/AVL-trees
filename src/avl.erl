%%%-------------------------------------------------------------------
%%% File    : avl.erl
%%% Author  : Andreas Hasselberg <andreas.hasselberg@gmail.com>
%%% Description : An implementation of avl trees.
%%%
%%% Created : 26 Dec 2007 by Andreas Hasselberg <>
%%%-------------------------------------------------------------------
-module(avl).

-define(depth(Node), element(3, Node)).
-define(nil, {nil, nil, 0}).

-export([empty/0, is_empty/1, size/1,
         lookup/2, get/2, is_defined/2,
         insert/3, update/3, enter/3,
         delete/2, delete_any/2, balance/1,
         keys/1, values/1, to_list/1, from_orddict/1,
         smallest/1, largest/1, take_smallest/1, take_largest/1,
         enters/2, inserts/2,
         iterator/1, next/1
        ]).

-export([depth/1]).

-ifdef(TEST).
-include_lib("proper/include/proper.hrl").
-endif.

%% node
%%
%% {
%%  key, %% term
%%  val, %% term
%%  depth, %% int
%%  left, %% node()
%%  right %% node()
%% }
%%

-type avl_node() :: {any(), any(), non_neg_integer(), avl_node(), avl_node()} | ?nil.
-opaque iter() :: [{any(), any(), avl_node()}].

-spec empty() -> avl_node().
empty() -> ?nil.

-spec is_empty(avl_node()) -> boolean().
is_empty(?nil) -> true;
is_empty(_) -> false.


%% the excecution time of size/1 is linear to the size of the tree
-spec size(avl_node()) -> non_neg_integer().
size(T) ->
    size1(T).

size1(?nil) -> 0;
size1({_Key, _Val, _Depth, TreeR, TreeL}) ->
    1 + size1(TreeR) + size1(TreeL).

-spec depth(avl_node()) -> non_neg_integer().
depth(T) -> ?depth(T).

-spec lookup(Key::any(), avl_node()) -> 'none' | {'value', any()}.
lookup(_FKey, ?nil) -> none;
lookup(FKey, {Key, Val, _, _, _} = T) ->
    lookup(FKey, T, Key, Val).

lookup(FKey, {Key, _V, _D, TreeL, _TR}, CKey, CVal) when FKey < Key ->
    lookup(FKey, TreeL, CKey, CVal);
lookup(FKey, {Key, Val, _D, _TL, TreeR}, _CKey, _CVal) ->
    lookup(FKey, TreeR, Key, Val);
lookup(FKey, ?nil, FKey, Val) -> {value, Val};
lookup(_, ?nil, _, _) -> none.

-spec get(Key::any(), avl_node()) -> Value::any().
get(Key, Tree) ->
    case lookup(Key, Tree) of
        {value, Val} -> Val;
        none -> erlang:error(badarg, [Key,Tree])
    end.

balance({_Key, _Val, _Dept, TreeL, TreeR} = T)
  when abs(?depth(TreeL) - ?depth(TreeR)) =/= 2 ->
    T;
balance({Key, Val, _Depth, {KL, VL, _DL, TLL, TLR}, TR}) %% ok for TL == ?nil
  when ?depth(TLL) =:= ?depth(TR)+1 -> %% LL inbalance
    NewDTR = max(?depth(TR), ?depth(TLR))+1, %% optimize?
    NewDL = max(NewDTR, ?depth(TLL))+1,
    {KL, VL, NewDL, TLL, {Key, Val, NewDTR, TLR, TR}};
balance({Key, Val, _Depth, TL, {KR, VR, _DR, TRL, TRR}})
  when ?depth(TRR) =:= ?depth(TL)+1 -> %% RR inbalance
    NewDTL = max(?depth(TL), ?depth(TRL))+1, %% optimize?
    NewDR = max(NewDTL, ?depth(TRR))+1,
    {KR, VR, NewDR, {Key, Val, NewDTL, TL, TRL}, TRR};
balance({K, V, _D, {KL, VL, _DL, TLL, {KLR, VLR, DLR, TLRL, TLRR}}, TR})
  when DLR =:= ?depth(TR)+1 -> %%LR inbalance / =:= D+1??
    DLL = ?depth(TLL),
    {KLR, VLR, DLL+2, {KL, VL, DLL+1, TLL, TLRL}, {K, V, DLL+1, TLRR, TR}};
balance({K, V, _D, TL, {KR, VR, _DR, {KRL, VRL, DRL, TRLL, TRLR}, TRR}})
  when DRL =:= ?depth(TL)+1 -> %%RL inbalance
    DRR = ?depth(TRR),
    {KRL, VRL, DRR+2, {K, V, DRR+1, TL, TRLL}, {KR, VR, DRR+1, TRLR, TRR}}.

%%done, not optimized
-spec insert(Key::any(), Val::any(), avl_node()) -> avl_node().
insert(AKey, AVal, {Key, Val, Depth, TreeL, TreeR}) when AKey < Key ->
    NewTreeL = insert(AKey, AVal, TreeL),
    NewT = {Key, Val, max(?depth(NewTreeL)+1, Depth), NewTreeL, TreeR},
    balance(NewT);
insert(AKey, AVal, {Key, Val, Depth, TreeL, TreeR}) when AKey > Key ->
    NewTreeR = insert(AKey, AVal, TreeR),
    NewT = {Key, Val, max(?depth(NewTreeR)+1, Depth), TreeL, NewTreeR},
    balance(NewT);
insert(AKey, AVal, ?nil) ->
    {AKey, AVal, 1, ?nil, ?nil};
insert(Key, Val, {Key, _, _, _, _} = Tree) ->
    erlang:error(badarg, [Key,Val,Tree]).


-spec update(Key::any(), Val::any(), avl_node()) -> avl_node().
update(AKey, AVal, {Key, Val, Depth, TreeL, TreeR}) when AKey < Key ->
    NewTreeL = update(AKey, AVal, TreeL),
    {Key, Val, Depth, NewTreeL, TreeR};
update(AKey, AVal, {Key, Val, Depth, TreeL, TreeR}) when AKey > Key ->
    NewTreeR = update(AKey, AVal, TreeR),
    {Key, Val, Depth, TreeL, NewTreeR};
update(Key, AVal, {Key, _Val, Depth, TreeL, TreeR}) ->
    {Key, AVal, Depth, TreeL, TreeR};
update(Key, Val, Tree) ->
    erlang:error(badarg, [Key,Val,Tree]).

-spec enter(Key::any(), Val::any(), avl_node()) -> avl_node().
enter(Key, Val, T) ->
    case is_defined(Key, T) of
        true  -> update(Key, Val, T);
        false -> insert(Key, Val, T)
    end.

-spec delete(Key::any(), avl_node()) -> avl_node().
delete(DKey, {Key, Val, _Depth, TreeL, TreeR}) when DKey < Key ->
    NewTreeL = delete(DKey, TreeL),
    NewT = {Key, Val, max(?depth(NewTreeL), ?depth(TreeR))+1, NewTreeL, TreeR},
    balance(NewT);
delete(DKey, {Key, Val, _Depth, TreeL, TreeR}) when DKey > Key ->
    NewTreeR = delete(DKey, TreeR),
    NewT = {Key, Val, max(?depth(TreeL), ?depth(NewTreeR))+1, TreeL, NewTreeR},
    balance(NewT);
delete(Key, {Key, _Val, _Depth, ?nil, TreeR}) -> TreeR;
delete(Key, {Key, _Val, _Depth, TreeL, ?nil}) -> TreeL;
delete(Key, {Key, _Val, _Depth, TreeL, TreeR})
  when ?depth(TreeL) > ?depth(TreeR) ->
    {LKey, LVal, NewTreeL} = take_largest(TreeL),
    T = {LKey, LVal, max(?depth(NewTreeL), ?depth(TreeR))+1, NewTreeL, TreeR},
    balance(T); %% Maybe not balance
delete(Key, {Key, _Val, _Depth, TreeL, TreeR}) ->
    {RKey, RVal, NewTreeR} = take_smallest(TreeR),
    T = {RKey, RVal, max(?depth(TreeL), ?depth(NewTreeR))+1, TreeL, NewTreeR},
    balance(T); %% Maybe not balance
delete(Key, ?nil) ->
    erlang:error(badarg, [Key, ?nil]).

%%XXX not so pretty
-spec delete_any(Key::any(), avl_node()) -> avl_node().
delete_any(Key, Tree) ->
    case catch delete(Key, Tree) of
        {'EXIT', _} -> Tree;
        NewTree when is_tuple(NewTree) ->
            NewTree
    end.

-spec is_defined(Key::any(), avl_node()) -> boolean().
is_defined(Key, T) ->
    case lookup(Key, T) of
        {value, _} -> true;
        none -> false
    end.

%%XXX stolen from gb_trees.
-spec keys(avl_node()) -> Keys::[any()].
keys(T) ->
    keys(T, []).

keys({Key, _Val, _Depth, TreeL, TreeR}, L) ->
    keys(TreeL, [Key | keys(TreeR, L)]);
keys(?nil, L) -> L.

%%XXX not sorted by values
-spec values(avl_node()) -> Values::[any()].
values(T) ->
    values(T, []).

values({_Key, Val, _Depth, TreeL, TreeR}, L) ->
    values(TreeL, [Val | values(TreeR, L)]);
values(?nil, L) -> L.

-spec to_list(avl_node()) -> [{Key::any(), Value::any()}].
to_list(T) ->
    to_list(T, []).

to_list({Key, Val, _Depth, TreeL, TreeR}, L) ->
    to_list(TreeL, [{Key, Val} | to_list(TreeR, L)]);
to_list(?nil, L) -> L.

%%XXX is in fact a from_list
-spec from_orddict([{Key::any(), Value::any()}]) -> avl_node().
from_orddict(List) ->
    Insert = fun({Key, Val}, TreeAcc) -> insert(Key, Val, TreeAcc) end,
    lists:foldl(Insert, empty(), List).

-spec take_largest(avl_node()) -> {Key::any(), Value::any(), avl_node()}.
%%tailcall?
take_largest({Key, Val, _Depth, TreeL, ?nil}) -> {Key, Val, TreeL};
take_largest({Key, Val, _Depth, TreeL, TreeR}) ->
    {LKey, LVal, NewTreeR} = take_largest(TreeR),
    NewT0 = {Key, Val, max(?depth(TreeL), ?depth(NewTreeR))+1, TreeL, NewTreeR},
    NewT = balance(NewT0),
    {LKey, LVal, NewT};
take_largest(?nil) ->
    erlang:error(badarg, [?nil]).


-spec take_smallest(avl_node()) -> {Key::any(), Value::any(), avl_node()}.
take_smallest({Key, Val, _Depth, ?nil, TreeR}) -> {Key, Val, TreeR};
take_smallest({Key, Val, _Depth, TreeL, TreeR}) ->
    {LKey, LVal, NewTreeL} = take_smallest(TreeL),
    NewT0 = {Key, Val, max(?depth(NewTreeL), ?depth(TreeR))+1, NewTreeL, TreeR},
    NewT = balance(NewT0),
    {LKey, LVal, NewT};
take_smallest(?nil) ->
    erlang:error(badarg, [?nil]).

-spec largest(avl_node()) -> {Key::any(), Value::any()}.
largest({Key, Val, _Depth, _TreeL, ?nil}) -> {Key, Val};
largest({_Key, _Val, _Depth, _TreeL, TreeR}) -> largest(TreeR);
largest(?nil) -> erlang:error(badarg, [?nil]).

-spec smallest(avl_node()) -> {Key::any(), Value::any()}.
smallest({Key, Val, _Depth, ?nil, _TreeR}) -> {Key, Val};
smallest({_Key, _Val, _Depth, TreeL, _TreeR}) -> smallest(TreeL);
smallest(?nil) -> erlang:error(badarg, [?nil]).

-spec enters(avl_node(), [{Key::any(), Value::any()}])-> avl_node().
enters(Tree, List) -> ops(Tree, List, fun enter/3).

-spec inserts(avl_node(), [{Key::any(), Value::any()}])-> avl_node().
inserts(Tree, List) -> ops(Tree, List, fun insert/3).

ops(Tree, List, F) ->
    OpF = fun({Key, Val}, TreeAcc) -> F(Key, Val, TreeAcc) end,
    lists:foldl(OpF, Tree, List).

-spec iterator(avl_node())-> iter().
iterator(Tree) ->
    iterator(Tree, []).

%% Acc is the nodes with respective right branches yet to traverse.
iterator({_Key, _Val, _Depth, ?nil, _TreeR} = T, Acc) -> %% no smaller element
    [T|Acc];
iterator({_Key, _Val, _Depth, TreeL, _TreeR} = T, Acc) -> %% walking down left
    iterator(TreeL, [T|Acc]);
iterator(?nil, Acc) -> Acc.

-spec next(Iter1::iter()) -> 'none' | {Key::any(), Val::any(), Iter2::iter()}.
next([{Key, Val, _Depth, _TreeL, TreeR} | As]) -> %% TreeL is allready consumed
    {Key, Val, iterator(TreeR, As)};
next([]) ->
    none.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

is_ok(?nil) -> true;
is_ok({_, _, 1, ?nil, ?nil}) -> true;
is_ok({Key, _, 2, ?nil,
       {KeyR, _, 1, ?nil, ?nil}}) ->
    Key < KeyR;
is_ok({Key, _, 2,
       {KeyL, _, 1, ?nil, ?nil},
       ?nil}) ->
    Key > KeyL;
is_ok({Key, _, Depth, TreeL, TreeR}) ->
    KeyL = element(1, TreeL),
    KeyR = element(1, TreeR),
    DepthL = ?depth(TreeL),
    DepthR = ?depth(TreeR),
    T = ((Key > KeyL) and
         (Key < KeyR) and
         (Depth > DepthL) and
         (Depth > DepthR) and
         ((Depth == DepthL+1) or
          (Depth == DepthR+1))),
    T and is_ok(TreeL) and is_ok(TreeR).

basic_test() ->
    A1 = empty(),
    ?assertEqual([], to_list(A1)),
    ?assertEqual(none, lookup(key, A1)),
    ?assertEqual(false, is_defined(key, A1)),
    ?assert(is_empty(A1)),
    A2 = insert(7, g, A1),
    ?assertEqual(A2, enter(7, g, A1)),
    A3 = insert(8, h, A2),
    A4 = insert(6, f, A3),
    ?assertEqual(1, avl:size(A2)),
    ?assertEqual(3, avl:size(A4)),
    ?assertNot(is_empty(A2)),
    ?assertEqual({value, h}, lookup(8, A4)),
    ?assertEqual(none, lookup(key, A4)),
    ?assertEqual(h, get(8, A4)),
    ?assertEqual({6, f}, smallest(A4)),
    ?assertEqual({8, h}, largest(A4)),
    ?assertEqual(true, is_defined(8, A4)),
    ?assertEqual({value, g}, lookup(7, A4)),
    ?assertEqual({value, f}, lookup(6, A4)),
    ?assert(is_ok(A1)),
    ?assert(is_ok(A2)),
    ?assert(is_ok(A3)),
    ?assert(is_ok(A4)).

lr_test() ->
    Add = fun({Key, Val}, AvlAcc) -> insert(Key, Val, AvlAcc) end,
    Stuff = [{10, "A"}, {5, "5"}, {13, "D"}, {1, "1"}, {7, "7"}, {6, "6"}],
    A2 = lists:foldl(Add, empty(), Stuff),
    ?assert(is_ok(A2)).

del_test() ->
    AddF = fun({Key, Val}, AvlAcc) -> insert(Key, Val, AvlAcc) end,
    DelF = fun(Key, AvlAcc) -> delete(Key, AvlAcc) end,
    Stuff = [{10, "A"}, {5, "5"}, {13, "D"}, {1, "1"}, {7, "7"}, {6, "6"}],
    T = lists:foldl(AddF, empty(), Stuff),
    ?assertEqual([1, 6, 7, 10, 13], keys(delete(5, T))),
    ?assertEqual([1, 5, 6, 7, 13], keys(delete(10, T))),
    ?assertEqual(["1", "5", "6", "7", "D"], values(delete(10, T))),
    KeySort = lists:keysort(1, Stuff),
    ?assertEqual(KeySort, iter_all(T)),
    ?assertEqual(KeySort, to_list(T)),
    ?assertEqual(T, from_orddict(KeySort)),
    ?assertEqual(?nil, from_orddict([])), %% silly
    DelStuff = [1, 6, 10],
    T2 = lists:foldl(DelF, T, DelStuff),
    ?assertEqual(depth(T2), depth(T)-1),
    ?assertEqual(T, delete_any(mojs, T)),
    ?assertNot(T =:= delete_any(5, T)),
    ?assert(is_ok(T)),
    ?assert(is_ok(T2)).

iter_all(Tree) ->
    iter_all(Tree, iterator(Tree)).

iter_all(Tree, Iter) ->
    case next(Iter) of
        none -> [];
        {Key, Val, NewIter} -> [{Key, Val}|iter_all(Tree, NewIter)]
    end.

take_largest_test() ->
    N = 15,
    List0 = [ {X, random:uniform(N)} || X <- lists:seq(1, N) ],
    List = lists:keysort(2, List0),
    Tree = inserts(empty(), List),
    ?assertMatch({N, _, _}, take_largest(Tree)),
    ?assertEqual(keys(element(3, take_largest(Tree))), lists:seq(1, N-1)).

take_smallest_test() ->
    N = 15,
    List0 = [ {X, random:uniform(N)} || X <- lists:seq(1, N) ],
    List = lists:keysort(2, List0),
    Tree = inserts(empty(), List),
    ?assertMatch({1, _, _}, take_smallest(Tree)),
    ?assertMatch({2, _, _}, take_smallest(element(3, take_smallest(Tree)))),
    ?assertEqual(keys(element(3, take_smallest(element(3, take_smallest(Tree))))),
                 lists:seq(3, N)).

%% proper_spec_test() ->
%%     ?assert(proper:check_specs(?MODULE)).

proper_prop_test() ->
    ?assert(proper:quickcheck(prop_insert_get())),
    ?assert(proper:quickcheck(prop_gb())),
    ?assert(proper:quickcheck(prop_enter_delete())).

%% ---------------------------------------------------------------------------
%% PROPERTY TESTS

%% FIXME: do something more interesting of this.
prop_insert_get() ->
    ?FORALL(X, any(),
            begin
                Y = get(X, insert(X,X, empty())),
                X =:= Y
            end).

prop_gb() ->
    ?FORALL(List,
            list({any(), any()}),
            begin
                Tree = enters(empty(), List),
                ?assert(is_ok(Tree)),
                GbInsert = fun({Key, Val}, TreeAcc) -> gb_trees:enter(Key, Val, TreeAcc) end,
                GbTree = lists:foldl(GbInsert, gb_trees:empty(), List),
%%                 io:format("Ops ~p res ~p ~p~n",
%%                           [List,
%%                            lists:sort(gb_trees:to_list(GbTree)),
%%                            lists:sort(to_list(Tree))]),
                lists:sort(gb_trees:to_list(GbTree)) =:= lists:sort(to_list(Tree))
            end).


prop_enter_delete() ->
    ?FORALL({Elem, List},
            {any(), list({any(), any()})},
            none =:= lookup(Elem, delete_any(Elem, enters(empty(), List)))).

-endif. %% TEST
