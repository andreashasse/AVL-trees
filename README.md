# AVL trees in erlang.

This is an avl tree implementation in erlang. It has the same interface as gb_trees. 
Perfomance is better for sorted input but worse for random input. 
The worse performance on random input is due to the fact the avl trees has stricter balancing conditions. 
In a lot of cases where the avl get unbalanced and need balancing the gb_trees is ok and there is a sigificant chance that gb_trees will be more balanced after the next operation.
The increased performace on ordered input is that the both trees need to balance a lot. The balancing of an avl tree takes less effort.

## Todo
### Complete
 - Borrow type specs from gb_trees.
 - Borrow interator and next.

### Performance
Would it improve performance to break the balancing of the tree when you know it's balanced?
 - Tree hight didn't change above this level
 - 2 balance has allready been made
