package scurry.lib.helpers

import scurry.rts._

object Normalise extends Module {
  def nf(exp: Expression) =
    Exp.oper("nf",Array(exp),t=>nf_(t))

  private def nf_(task: Task): List[Task] = {
    matchArg(task, 0, (name,isNF,args) => {
      if (isNF) {
        task.exp.set(Constructor(name,true),args)
        Nil
      } else if (args.exists(x=>x.isFailure)) {
        task.exp.become(Exp.failure)
        Nil
      } else if (args.forall(x=>x.isNormalised)) {
        task.exp.args(0).set(Constructor(name,true),args)
        task.exp.set(Constructor(name,true),args)
        Nil
      } else {
        task.setDeps(args.length)
        args.map(x => new Task(nf(x),task)).toList
      }
    })
  }
}
