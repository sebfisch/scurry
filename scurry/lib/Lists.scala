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

  def length(l: Expression): Expression =
    Exp.oper("length",Array(l),t => length_(t))

  private def length_(task: Task) = {
    matchArg(task,0, (name,_,_) => {
      name match {
        case Empty_ => ret(task,Integer.value(0))
        case Cons_ => {
          val l = task.exp.args(0).args(1)
          ret(task,Integer.plus(Integer.value(1),length(l)))
        }
      }
    })
  }

  def take(n:Expression,l: Expression): Expression =
    Exp.oper("take",Array(n,l),t=>take_(t))

  private def take_(task: Task) = {
    matchArgs(task,Nil,args => {
      args match {
        case _ :: Empty_ :: Nil =>
          retCons(task,Constructor(Empty_,true),Array())
        case Integer.Integer_(n) :: Cons_ :: Nil => {
          if (n==0) retCons(task,Constructor(Empty_,true),Array())
          else {
            val m = task.exp.args(0)
            val x = task.exp.args(1).args(0)
            val l = task.exp.args(1).args(1)
            ret(task, Cons(x,take(Integer.minus(m,Integer.value(1)),l)))
          }
        }
        case _ => Nil
      }
    })
  }

  def drop(n: Expression, l: Expression): Expression =
    Exp.oper("drop",Array(n,l),t=>drop_(t))

  private def drop_(task: Task) = {
    matchArgs(task,Nil,args => {
      args match {
        case _ :: Empty_ :: Nil =>
          retCons(task,Constructor(Empty_,true),Array())
        case Integer.Integer_(n) :: Cons_ :: Nil => {
          if (n==0) ret(task,task.exp.args(1))
          else {
            val m = task.exp.args(0)
            val l = task.exp.args(1).args(1)
            ret(task, drop(Integer.minus(m,Integer.value(1)),l))
          }
        }
        case _ => Nil
      }
    })
  }

  def enumFrom(n: Expression): Expression =
    Exp.oper("enumFrom",Array(n),t => enumFrom_(t))

  private def enumFrom_(task: Task) = {
    val arg = task.exp.args(0)
    ret(task,Cons(arg, enumFrom(Integer.plus(arg,Integer.value(1)))))
  }

  def enumFromTo(m: Expression, n: Expression) =
    Exp.oper("enumFromTo",Array(m,n),t => enumFromTo_(t))

  def enumFromTo_(task: Task) = {
    val m = task.exp.args(0)
    val n = task.exp.args(1)
    ret(task,take(Integer.plus(Integer.minus(n,m),Integer.value(1)),
                  enumFrom(m)))
  }
}
