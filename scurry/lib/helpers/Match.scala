package scurry.lib.helpers

import scurry.rts._

object Match {
  def one(task: Task, index: Int, 
          replace: (ConsName,Array[Expression]) => List[Task]) = {
    val arg = task.exp.args(index)
    arg.kind match {
      case Failure => { 
        task.exp.become(Exp.failure)
        Nil
      }
      case Constructor(name,_) => replace(name,arg.args)
      case Operation(_,_) => {
        task.setDeps(1)
        List(new Task(arg,task))
      }
    }
  }
}