package scary.test

import scary.rts._
import scary.lib._

object SmallExamples {
  def main(args: Array[String]) = {
    triple_not
    single_isEmpty
    project_cycle
    shared_not
  }

  private def eval_print(exp: Expression) {
    DepthFirstEvaluator.execute(exp)
    println(exp)
  }

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
}
