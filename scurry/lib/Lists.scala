package scurry.lib

import scurry.rts._
import scurry.lib.helpers._

object Lists extends Module {

  case object Empty_ extends ConsName
  case object Cons_  extends ConsName

  def Empty = Exp.cons(Empty_,Array())
  def Cons(x: Expression,xs: Expression) = Exp.cons(Cons_,Array(x,xs))

  def isEmpty(l: Expression) =
    Exp.oper("null",Array(l),t=>isEmpty_(t))

  private def isEmpty_(task: Task): List[Task] = {
    matchArg(task, 0, (name,_,_) => {
      val res = name match {
        case Empty_ => Bool.True_
        case Cons_  => Bool.False_
      }
      retCons(task, Constructor(res,true), Array())
    })
  }

  def head(l: Expression) =
    Exp.oper("head",Array(l),t=>head_(t))

  private def head_(task: Task): List[Task] = {
    matchArg(task,0,(name,_,_) => {
      val exp = task.exp
      name match {
        case Cons_ => ret(task,exp.args(0).args(0))
        case _     => ret(task,Exp.failure)
      }
    })
  }

  def tail(l: Expression) =
    Exp.oper("tail",Array(l),t=>tail_(t))

  private def tail_(task: Task): List[Task] = {
    matchArg(task,0,(name,_,_) => {
      val exp = task.exp
      name match {
        case Cons_ => ret(task,exp.args(0).args(1))
        case _     => ret(task,Exp.failure)
      }
    })
  }

  def repeat(x: Expression) =
    Exp.oper("repeat",Array(x),t=>repeat_(t))

  private def repeat_(task: Task): List[Task] = {
    var l = Lists.Cons(task.exp.args(0),null)
    l.args.update(1,task.exp); // or l instead of task.exp
    ret(task,l)
  }
}
