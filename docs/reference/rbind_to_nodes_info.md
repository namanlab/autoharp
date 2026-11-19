# Update node information.

Updates the node information regarding an R expression.

## Usage

``` r
rbind_to_nodes_info(id, name, call_status, formal_arg, depth, env_ni)
```

## Arguments

- id:

  The id of the node to be added. This should be an integer of length 1.

- name:

  The name of the node.

- call_status:

  Is the language object a call or a symbol/literal? This should a
  logical value.

- formal_arg:

  Is the language object a formal argument or not? This should be a
  logical value.

- depth:

  An integer indicating the depth of this language object in the parse
  tree.

- env_ni:

  An environment object, possibly containing a data frame with columns
  id, name, call_status, formal_arg and depth.

## Value

TRUE is returned invisibly.

## Details

This is for internal use. It may be removed from user-view soon!
