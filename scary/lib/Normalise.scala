package scary.lib

import scary.rts._

object Normalise {
  def nf(task: Task): List[Task] = {
    val arg = task.exp.args(0);
    arg.kind match {
      case Constructor(name,isNF) => {
        task.exp.set(Constructor(name,true),arg.args)
        if (isNF) Nil
        else arg.args.map(x => new Task(nf_call(x),null)).toList
      }
      case Operation(_,_) => {
        task.setDeps(1)
        List(new Task(arg,task))
      }
    }
  }

  def nf_call(exp: Expression) =
    Exp.oper("nf",Array(exp),task => nf(task))
}
