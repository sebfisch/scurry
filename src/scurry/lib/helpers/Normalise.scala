package scurry.lib.helpers

import scurry.rts._

abstract class ExpIterator extends Iterator[Expression] {
  def putNext(exp: Expression)
}

object Normalise extends Module {
  def hnf(master: ExpIterator, exp: Expression) =
    Exp.oper("hnf",Array(exp),t=>hnf_(master,t))

  private def hnf_(master: ExpIterator, task: Task): List[Task] = {
    matchArg(task, 0, (_,_,_) => { 
      master.putNext(task.exp.args(0))
      Nil
    })
  }

  def nf(exp: Expression) =
    Exp.oper("nf",Array(exp),t=>nf_(t))

  private def nf_(task: Task): List[Task] = {
    matchArg(task, 0, (name,isNF,args) => {
      if (args.exists(x=>x.isFailure)) {
        task.exp.become(Exp.failure)
        Nil
      } else if (isNF) {
        task.exp.set(Constructor(name,true),args)
        Nil
      } else {
        val consexp = task.exp.args(0)
        consexp.setKind(Operation(name.toString,
                                  t=>nf_cons_(Constructor(name,true),t)))
        consexp.setArgs(args.map(x => nf(x)))
        val ts = newTask(consexp,task).toList
        task.setDeps(ts.length)
        ts
      }
    })
  }

  private def nf_cons_(kind: ExpKind, task: Task) = {
    matchArgs(task,Nil,_ => retCons(task,kind,task.exp.args))
  }
}
