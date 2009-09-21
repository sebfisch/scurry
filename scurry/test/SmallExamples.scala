package scurry.test

import scurry.rts._
import scurry.lib._

object SmallExamples {
  def main(args: Array[String]) = {
    triple_not
    single_isEmpty
    project_cycle
    shared_not
    nested_fail
    failing_cycle
  }

  private def eval_print(exp: Expression) =
    // use LifoQ or depth-first evaluation of redexes, FifoQ for breadth-first
    println(new SequentialEvaluator(new FifoQ()).execute(exp))

  private def triple_not =
    eval_print(Bool.not(Bool.not(Bool.not(Bool.False))))

  private def single_isEmpty =
    eval_print(Lists.Cons(Bool.not(Lists.isEmpty(Lists.Empty)),Lists.Empty))

  private def project_cycle =
    eval_print(Lists.head(Lists.tail(Lists.repeat(Bool.True))))

  private def shared_not = {
    val x = Bool.not(Bool.False)
    eval_print(Lists.Cons(x,Lists.Cons(x,Lists.Empty)))
  }

  private def nested_fail =
    eval_print(Lists.Cons(Lists.head(Lists.Empty),Lists.Empty))

  // needs bfs
  private def failing_cycle = {
    val l = Lists.Cons(Lists.head(Lists.Empty),null)
    l.args.update(1,l)
    eval_print(l)
  }
}
