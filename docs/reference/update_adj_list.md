# Update adjacency list.

Updates the adjacency list for an R expression parse tree.

## Usage

``` r
update_adj_list(
  update_type = c("new_node", "add_child"),
  node_id,
  node_name,
  child_node,
  env_ni
)
```

## Arguments

- update_type:

  This should be either "new_node" or "add_child". If it is a new node,
  an empty list component is added. If it is add child, then child_node
  should be provided too.

- node_id:

  An integer.

- node_name:

  The name of the new node to be added. This must be provided if the
  update_type is "new_node".

- child_node:

  An integer.

- env_ni:

  An environment object, possibly containing an adjacency list that will
  later be used to construct a TreeHarp object.

## Value

An invisible TRUE is returned.

## Details

This is for internal use. It may be removed from user-view soon!
