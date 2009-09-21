package scurry.lib

import scurry.rts._
import scurry.lib.helpers._

object Bool {

  case object True_  extends ConsName
  case object False_ extends ConsName

  def True  = Exp.cons(True_ ,Array())
  def False = Exp.cons(False_,Array())
  
  def not(arg: Expression) =
    Exp.oper("not",Array(arg),task => not_(task))

  private def not_(task: Task) = {
    Match.one(task, 0, (name,_,_) => {
      val res = name match {
        case True_ => False_
        case False_ => True_
      }
      task.exp.set(Constructor(res,true),Array())
      Nil
    })
  }
}  

