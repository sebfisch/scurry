package scurry.lib.helpers

import scurry.rts._

object Normalise {
  def nf(exp: Expression) =
    Exp.oper("nf",Array(exp),t=>nf_(t))

  private def nf_(task: Task): List[Task] = {
    val arg = task.exp.args(0);
    arg.kind match {
      case Failure => {
        task.exp.become(Exp.failure)
        Nil
      }
      case Constructor(name,isNF) => {
        if (isNF) {
          task.exp.set(Constructor(name,true),arg.args)
          Nil
        } else if (arg.args.forall(x=>x.isNormalised)) {
          arg.set(Constructor(name,true),arg.args)
          task.exp.set(Constructor(name,true),arg.args)
          Nil
        } else if (arg.args.exists(x=>x.isFailure)) {
          task.exp.become(Exp.failure)
          Nil
        } else {
          task.setDeps(arg.args.length)
          arg.args.map(x => new Task(nf(x),task)).toList
        }
      }
      case Operation(_,_) => {
        task.setDeps(1)
        List(new Task(arg,task))
      }
    }
  }
}
