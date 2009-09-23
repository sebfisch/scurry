package scurry.lib.helpers

import scurry.rts._

class Module {
  def ret(task: Task, result: Expression) = {
//     println("replacing " + task.exp.kind + " with " + result.kind)
    task.exp.become(result)
//     println("done replacing")
    if (result.isHeadNormalised) Nil else List(task)   
  }

  def retCons(task: Task, kind: ExpKind, args: Array[Expression]) = {
    task.exp.set(kind,args)
    Nil
  }

  def matchArg(task: Task, index: Int, 
               replace: (ConsName,Boolean,Array[Expression]) => List[Task]) = {
    val arg = task.exp.args(index)
    arg.kind match {
      case Failure => { 
        task.exp.become(Exp.failure)
        Nil
      }
      case Choice => {
        arg.simplifyChoice
        arg.args.partition(e => e.isHeadNormalised) match {
          case (hnfs,es) => {
            if (hnfs.isEmpty) {
              task.setDeps(1)
              es.map(e => new Task(e,task)).toList
            } else {
              var exp = task.exp
              var ts = hnfs.map(h => {
                var t = task.copy
                t.exp.args.update(index,h)
                t
              })
              var args = ts.map(t => t.exp)
              if (!es.isEmpty) {
                val e = if (es.length == 1) es(0)
                        else Exp.simple_choice(es.toArray)

                val copy = task.exp.copy
                copy.args.update(index,e)
                args = args ++ Array(copy)
                task.setExp(copy)
                if (task.deps.isEmpty)
                  ts = ts ++ Array(task)
                else
                  task.setDeps(1)
              }
              exp.become(Exp.simple_choice(args.toArray))
              ts.toList
            }
          }
        }
      }
      case Constructor(name,isNF) => replace(name,isNF,arg.args)
      case Operation(_,_) => {
        task.setDeps(1)
        List(new Task(arg,task))
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
    var choiceIndex = exps.findIndexOf(e => e.isChoice)
    if (exps.exists(e => e.isFailure)) {
      task.exp.become(Exp.failure)
      Nil
    } else if (choiceIndex >= 0) {
      matchArg(task,choiceIndex, (_,_,_) => List(task))
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
