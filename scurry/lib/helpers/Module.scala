package scurry.lib.helpers

import scurry.rts._

class Module {
  def ret(task: Task, result: Expression) = {
    task.exp.become(result)
    if (result.isHeadNormalised) Nil else List(task)    
  }

  def retCons(task: Task, kind: ExpKind, args: Array[Expression]) = {
    task.exp.set(kind,args)
    Nil
  }

  def matchArg(task: Task, index: Int, 
               replace: (ConsName,Boolean,Array[Expression]) => List[Task]) = {
    val exp = task.exp.args(index)
    exp.kind match {
      case Failure => { 
        task.exp.become(Exp.failure)
        Nil
      }
      case Constructor(name,isNF) => replace(name,isNF,exp.args)
      case Operation(_,_) => {
        task.setDeps(1)
        List(new Task(exp,task))
      }
    }
  }

  def matchArgs(task: Task, indices: List[Int], 
                replace: List[ConsName] => List[Task]) =
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
        case Constructor(name,isNF) => name
      }))
    } else {
      val ops = exps.filter(e => e.isOperation)
      task.setDeps(ops.length)
      ops.map(e => new Task(e,task))
    }
  }
}
