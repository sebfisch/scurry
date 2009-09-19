package scurry.lib

import scurry.rts._
import scurry.lib.helpers._

object Lists {

  case object Empty_ extends ConsName
  case object Cons_  extends ConsName

  def Empty = Exp.cons(Empty_,Array())
  def Cons(x: Expression,xs: Expression) = Exp.cons(Cons_,Array(x,xs))

  def isEmpty(l: Expression) =
    Exp.oper("null",Array(l),t=>isEmpty_(t))

  private def isEmpty_(task: Task): List[Task] = {
    Match.one(task, 0, (name,_) => {
      val res = name match {
        case Empty_ => Bool.True_
        case Cons_  => Bool.False_
      }
      task.exp.set(Constructor(res,true), Array())
      Nil
    })
  }

  def head(l: Expression) =
    Exp.oper("head",Array(l),t=>head_(t))

  private def head_(task: Task): List[Task] = {
    Match.one(task,0,(name,_) => {
      val exp = task.exp
      name match {
        case Cons_ => {
          exp.become(exp.args(0).args(0))
          Nil
        }
        case _ => {
          exp.become(Exp.failure)
          Nil
        }
      }
    })
  }

  def tail(l: Expression) =
    Exp.oper("tail",Array(l),t=>tail_(t))

  private def tail_(task: Task): List[Task] = {
    Match.one(task,0,(name,_) => {
      val exp = task.exp
      name match {
        case Cons_ => {
          exp.become(exp.args(0).args(1))
          Nil
        }
        case _ => {
          exp.become(Exp.failure)
          Nil
        }
      }
    })
  }

  def repeat(x: Expression) =
    Exp.oper("repeat",Array(x),t=>repeat_(t))

  private def repeat_(task: Task): List[Task] = {
    var l = Lists.Cons(task.exp.args(0),null)
    l.args.update(1,task.exp); // or l instead of task.exp
    task.exp.become(l)
    Nil
  }
}
