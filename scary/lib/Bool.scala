package scary.lib

import scary.rts._

object Bool {

  case object True_  extends ConsName
  case object False_ extends ConsName

  def True  = Exp.cons(True_ ,Array())
  def False = Exp.cons(False_,Array())
  
  def not(arg: Expression) =
    Exp.oper("not",Array(arg),task => not_(task))

  private def not_(task: Task) = {
    val arg = task.exp.args(0);
    arg.kind match {
      case Constructor(name,_) => {
        val res = name match {
          case True_ => False_
          case False_ => True_
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
}  

