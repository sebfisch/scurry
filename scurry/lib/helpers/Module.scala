package scurry.lib.helpers

import scurry.rts._

class Module {
  def newTask(exp: Expression, parent: Task) =
    /*if (exp.isActive) None else*/ {
      exp.setActive
      Some(new Task(exp,parent))
    }

  def ret(task: Task, result: Expression) = {
    task.exp.become(result)
    retTasks(task,result)
  }

  private def retTasks(task: Task, exp: Expression): List[Task] = {
    exp.kind match {
      case Operation(_,_) => newTask(exp,task.parent).toList
//       case Choice => exp.args.flatMap(e => retTasks(task,e)).toList
      case _ => Nil
    }
  }

  def retCons(task: Task, kind: ExpKind, args: Array[Expression]) = {
    task.exp.set(kind,args)
    Nil
  }

  def matchArg(task: Task, index: Int, 
               replace: (ConsName,Boolean,Array[Expression]) => List[Task]) = {
    val arg = task.exp.args(index)
    arg.simplifyChoice
    arg.kind match {
      case Failure => { 
        task.exp.become(Exp.failure)
        Nil
      }
      case Choice => {
        arg.args.partition(e => e.isHeadNormalised) match {
          case (hnfs,es) => {
            if (hnfs.isEmpty) {
              task.setDeps(1)
              es.flatMap(e => newTask(e,task)).toList
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
                ts = ts ++ newTask(copy,task.parent)
                task.cancel // surprisingly tricky to reuse task safely
                /* Here, we destroy the parent of a possibly long chain of
                 * already executed tasks. If the end of this chain is picked
                 * before the newly created task this is no problem, but if not
                 * work is duplicated. The reason why we destroy the task is
                 * that we cannot be sure that all operation rooted sub-terms
                 * of the rotated choice are already active. Especially, when
                 * a function returns a choice where one argument is in HNF
                 * but the other is not, then it does not create a task for the
                 * op-rooted alternative. It returns an empty list of tasks
                 * to reenable the parent to be executed. */
              }
              exp.become(Exp.simple_choice(args.toArray))
              ts.toList
            }
          }
        }
      }
      case Constructor(name,isNF) => replace(name,isNF,arg.args)
      case Operation(_,_) => {
        val ts = newTask(arg,task).toList
        task.setDeps(ts.length)
        ts
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
      val ts = exps.flatMap(e => if (e.isOperation) newTask(e,task) else None)
      task.setDeps(ts.length)
      ts
    }
  }
}
