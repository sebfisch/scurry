package scurry.lib.helpers

import scurry.rts._

object Match {
  def one(task: Task, index: Int, 
          replace: (ConsName,Boolean,Array[Expression]) => List[Task]) = {
    val arg = task.exp.args(index)
    arg.kind match {
      case Failure => { 
        task.exp.become(Exp.failure)
        Nil
      }
      case Constructor(name,isNF) => replace(name,isNF,arg.args)
      case Operation(_,_) => {
        task.setDeps(1)
        List(new Task(arg,task))
      }
    }
  }

  def args(task: Task, indices: List[Int], 
           replace: List[(ConsName,Boolean,Array[Expression])] => List[Task]) =
  {
    // indices lists the arguments to be matched
    // indices.isEmpty indicates that all arguments are to be matched 
    val exps: List[Expression] =
      if (indices.isEmpty) task.exp.args.toList
      else indices.map(i => task.exp.args(i))
    if (exps.exists(e => e.isFailure)) {
      task.exp.become(Exp.failure)
      Nil
    } else if (exps.forall(e => e.isHeadNormalised)) {
      replace(exps.map(e => e.kind match {
        case Constructor(name,isNF) => (name,isNF,e.args)
      }))
    } else {
      val ops = exps.filter(e => e.isOperation)
      task.setDeps(ops.length)
      ops.map(e => new Task(e,task))
    }
  }
}
