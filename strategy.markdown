Expressions
===========

Expressions are of the form `c(e1,...,en)`, `f(e1,...,en)`, or
`?(e1,...,en)` where `c` is a constructor, `f` the name of a defined
operation, and `?` denotes nondeterministic choice; 0 <= n; and `e1`,
... , `en` are expressions. Constructors, operations, as well as
choices may have zero arguments and a choice with zero arguments
represents failure. (We may omit the parenthesis in case of zero
arguments and write `f` instead of `f()`.) Expressions may be shared
so they are arbitrary graphs not trees.

Computing head-normal forms
===========================

A head-normal form (hnf) is a constructor-rooted expression. An
expression that is not in hnf may be (1) choice- or (2) operation
rooted. The following *steps* are possible in order to compute hnfs:

  1. To compute hnfs in choice rooted expressions one can
       * merge nested choices, or
       * make a step on an operation-rooted alternative

     Constructor-rooted alternatives of choices are hnfs.

  2. To compute hnfs of operation rooted expressions `e` one can
       * perform a reduction step, if `e` is a redex
       * reduce to `?()` if any needed subexpression is `?()`
       * perform a step on an operation-rooted needed subexpression
       * perform a step on an operation-rooted argument of a needed choice
       * distribute the root over a needed choice with
         constructor-rooted children

Distributing operations over a choice
-------------------------------------

An operation `f` can be distributed over a needed choice with at least
one constructor-rooted child. The following example (numbers are
considered constructor terms) gives the intuition and can be easily
generalised:

    +(?(1,2),+(3,4))  ==>  ?(+(1,+(3,4)),+(2,+(3,4)))

In order to make the sharing more explicit, we can write the
expressions as graphs:

         +                       ?
       /   \                  /     \
      ?     +                +       +
     / \   / \     ==>      / \     / \
    1   2 3   4            1   \   2   |
                                \     /
                                   +
                                  / \
                                 3   4

The distribution step creates a new choice, all alternatives are
rooted by the distributed operation, and its arguments are updated
according to the alternatives of the original choice. 

This operation may destroy sharing of choices and, hence, does not
adhere to call-time choice semantics. For example, the following is
also a valid distribution.

        +                    ?
       / \                /     \
       \ /               +       +
        ?      ==>      / \     / \
       / \             0   \   1   |
      0   1                 \     /
                               ?
                              / \
                             0   1

The hnfs of the left expression are `?(0,2)`, the hnfs of the right
expression are `?(0,1,1,2)`. (In fact, this depiction is inaccurate to
increase readability: the different occurrences of `0` and `1` should
be shared).

Computation of normal forms
===========================

A normal form (nf) contains only constructors. To compute the nfs of
an expression we compute its hnfs. To compute the nf of a
constrctor-rooted expression we can

  1. compute nfs of its arguments, or
  2. distribute the root constructor over a choice which has at least one
     alternative in nf.

