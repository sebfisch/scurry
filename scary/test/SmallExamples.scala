package scary.test

import scary.rts._
import scary.lib._

object SmallExamples {
  def main(args: Array[String]) = {
    triple_not
  }

  private def triple_not = {
    var exp = Bool.not(Bool.not(Bool.not(Bool.False)))
    DepthFirstEvaluator.execute(exp)
    println(exp)
  }
}
