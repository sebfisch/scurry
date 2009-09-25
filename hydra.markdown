This file describes an idea how to implement the strategy in
`strategy.markdown` and lists *challenges* that we encountered during
our implementation.

The hydra
=========

In order to keep track of evaluation dependencies, we intended to use
a *stack with many heads* that Sergio gave the telling name *hydra*.

Stacks can be used to model lazy function evaluation. The function
call on top of the stack is the current target of evaluation and if it
needs to evaluate sub expressions to make reduction possible, these
sub expressions are put on top of the original function call.

By using a stack like this, the order of evaluation of the needed
sub expressions is fixed by the order in which they are put on the
stack. In order to allow more flexible evaluation strategies one can
put the needed sub expressions *next to* each other *on top of* the
function call that needs them. As a consequence the stack is a tree
rather than a list. It becomes a hydra. The run-time system can
continue with any head of the hydra using different sequential
strategies or in parallel.

Here is a tiny example. The following depicts the evaluation of
`+(+(1,2),+(3,4))` using a hydra. In each step, The current heads are
marked with an asterisk `*`.


         +*                +                 +*             10
       /   \             /   \             /   \
      +     +    ==>    +*    +*   ==>    3     7    ==>
     / \   / \         / \   / \
    1   2 3   4       1   2 3   4

Both arguments of `+` are needed and thus placed on the hydra as new
heads next to each other on top of the `+` at the root which is no
head anymore. Above, we have evaluated both arguments in parallel and
after removing the evaluated heads, the original call to `+` becomes a
head again. We can implement this by storing a queue of current heads
which link to their *parent* that has demanded their
evaluation. Additionally, every element of the hydra stores a counter
that counts how many needed sub expressions have yet to be
evaluated. When a head is evaluated, it decrements the counter of its
parent which becomes a head again, once its counter reaches zero,
signifying that no needed evaluations are pending.

Expressions can have more than one parent if they are shared. In the
expression

         +
       /   \
      +     +
     / \   / \
    1    +    4
        / \
       2   3

The `+` applied to `2` and `3` has two parents and decrements the
counters of both once it is evaluated to `5`.

Nondeterministic choice
=======================

Challenge 1: counter invariant
------------------------------

In presence of nondeterministic choice, there are alternatives for
satisfying need. The `+` function in the following example could
continue when either of the calls to `fib` and the call to `akk` is
done.

           +
        /     \
       ?      akk
     /   \     |
    fib  fib   5
     |    |
     7    4

Any call to `fib` needs to notify `+` if it is done such that `+` can
consume the result. But if both calls to `fib` notify `+` by
decrementing its counter before `akk` does, it reaches zero although
`akk` is not evaluated. The counter no longer counts the number of
needed dependencies.

To reinforce this invariant, we can treat choices such that they
decrement the counter of their parents only once when some child
decrements the choice counter. This could be implemented by recording
in choices whether one of their descendants has finished evaluation,
i.e., is in head-normal form. If a choice is *notified* that one of
its descendants has been evaluated, it only propagates this
information to its parents if there has not been an evaluated child
before. We can delete all parents of a choice node after notifying
them about the hnf descendant. As a consequence, a choice where the
flag for hnf descendants is set never has any parents. This property
will become important later. In fact, we can also delete the parents
of operation-rooted nodes once they have been
notified. Constructor-rooted expressions never have parents.

Function calls that are activated as head of the hydra check all their
needed arguments and activate only those as heads that are
operation-rooted or a choice without hnf descendants.

If a choice is activated as head of the hydra, it checks the flag
which tells whether one of its arguments is evaluated. If it is set it
deactivates itself. If it is not set, it checks whether one of its
children is constructor-rooted or a choice with hnf descendants. If
so, it sets its flag, decrements parent counters, and deactivates
itself. If not, it activates all its children as new heads and
deactivates itself. Once a choice is notified by one of its children
it is activated as head again.

Challenge 2: choice distribution
--------------------------------

When an operation-rooted expression is on top of the hydra and one
needed argument is a choice with hnf descendants then the operation
can distribute over the choice. This distribution is more complex as
one might expect because parents need to be maintained and new heads
of the hydra need to be created carefully.

Distributing over a choice changes an expression in the following way:

           +                      ?
        /     \                /     \
       ?      akk             +       +
     /   \     |             / \     / \
    fib   3    5    ==>    fib  \   3   |
     |                      |    \     /
     7                      7      akk
                                    |
                                    5

The `+` node is replaced with a `?` node, its children are two new `+`
expressions with the original choice alternatives in place of the
original choice argument. We know that the original choice has an hnf
descendant (otherwise we would not have been allowed to distribute)
and, hence, has no parents. Also at least one alternative of the
choice is constructor-rooted or a choice with hnf descendants.

The newly created `?` node retains the parents of the original `+`
node, its hnf flag is not set, and it also retains the flag that
identifies it as part of the hydra. We will see shortly, that such a
flag is needed to avoid adding nodes as hydra heads more than once.

According to parents we make the following modifications:

  * the newly created `+` nodes have the root node as parent
  * in the original choice arguments that are now arguments of the new 
    `+` nodes, we replace parent edges to the original `?` with parent
    edges to the corresponding new `+` node. If they don't have a parent
    edge to the `?` node, no parent edge to the `+` node is added.

The new `+` nodes are initialised according to the following rules:

  * if the new argument is a constructor then the `+` node is marked
    as part of the hydra and returned as new head
  * if the new argument is operation-rooted and part of the hydra then
    the dependency counter of the new `+` node (which is zero) is set to
    one and it is no head of the hydra
  * if the new argument is operation-rooted and not part of the hydra then
    the new `+` node is a new head
  * if the new argument is choice-rooted without hnf descendants and part
    of the hydra then we set the dependency counter of the new `+` node
    to one and it is no head of the hydra
  * if the new argument is choice-rooted with hnf descendants or not part
    of the hydra then the new `+` node is a new head

Differences to strategy
=======================

Compared to the strategy in `strategy.markdown` the sketched
implementation with a hydra has the following differences:

  * choices are not flattened and always binary.
  * failure in needed arguments does not necessarily lead to failure of a
    function call if some other needed argument loops because a function
    call is only reconsidered when the dependency counter is zero.
    If a needed argument is failure before we put dependencies on the hydra
    then the sketched implementation can fail.

Finite Failure
==============

We use a special constructor for failure because we only have binary
choices. Failure can be handled by considering it when collecting
needed arguments in function calls and when processing choices as
hydra heads.

Experimental idea
-----------------

Instead of using a counter, function calls could also check whether
they are reducible whenever they are notified by a needed
argument. This seems to introduce unnecessary checks but would allow
to detect finite failure in one needed argument and cause finite
failure of the call that needs it.

In order to avoid reduction steps on other needed arguments if a
needed argument fails, a function call must notify activated children
that they are no longer needed. They can then remove the associated
parent from their list of parents and mark themselves as *orphan* if
they do not have parents left. Orphans disappear from the hydra when
they are executed and can be re-enabled when they are demanded again.

Handling orphans is tricky, however: Orphans do not need to be made
new heads of the hydra if they already are. It suffices to reset their
status to active. If an orphan is no head of the hydra and its needed
children are orphans too then it needs to reset the status of its
needed children to active too.
