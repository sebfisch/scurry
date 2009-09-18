package examples

import scary._

object Not {
  def not(task: Task): List[Task] = {
    val arg = task.exp.args(0);
    arg.kind match {
      case Constructor(name,_) => {
        val res = name match {
          case "true" => "false"
          case "false" => "true"
        }
        task.exp.set(Constructor(res,true),new Array(0))
        Nil
      }
      case Operation(_,_) => {
        task.setDeps(1)
        List(new Task(arg,task))
      }
    }
  }

  private def not_call(arg: Expression) =
    Exp.oper("not",Array(arg),node => not(node))

  def main(args: Array[String]) = {
    var arg = Exp.cons("false",Array())
    var exp = not_call(not_call(not_call(arg)))
    DepthFirstEvaluator.hnf(exp)
    println(exp)
  }
}  

