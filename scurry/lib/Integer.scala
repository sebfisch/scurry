package scurry.lib

import scurry.rts._
import scurry.lib.helpers._

object Integer extends Module {

  case class Integer_(value: Int) extends ConsName {
    override def toString = value.toString + "_"
  }

  def value(n: Int) = Exp.cons(Integer_(n) ,Array())
  
  def negate(x: Expression) =
    Exp.oper("negate",Array(x),prim(n => Integer_(-n)))
  
  def abs(x: Expression) =
    Exp.oper("abs",Array(x),prim(n => Integer_(n.abs)))

  def plus(x: Expression, y: Expression) =
    Exp.oper("+",Array(x,y),prim2((n,m) => Integer_(n+m)))

  def minus(x: Expression, y: Expression) =
    Exp.oper("-",Array(x,y),prim2((n,m) => Integer_(n-m)))

  def times(x: Expression, y: Expression) =
    Exp.oper("*",Array(x,y),prim2((n,m) => Integer_(n*m)))

  def div(x: Expression, y: Expression) =
    Exp.oper("div",Array(x,y),prim2((n,m) => Integer_(n/m)))

  def mod(x: Expression, y: Expression) =
    Exp.oper("mod",Array(x,y),prim2((n,m) => Integer_(n%m)))

  def lt(x: Expression, y: Expression) =
    Exp.oper("<",Array(x,y),prim2(
      (n,m) => if (n<m) Bool.True_ else Bool.False_))

  def leq(x: Expression, y: Expression) =
    Exp.oper("<=",Array(x,y),prim2(
      (n,m) => if (n<=m) Bool.True_ else Bool.False_))

  def gt(x: Expression, y: Expression) =
    Exp.oper(">",Array(x,y),prim2(
      (n,m) => if (n>m) Bool.True_ else Bool.False_))

  def geq(x: Expression, y: Expression) =
    Exp.oper(">=",Array(x,y),prim2(
      (n,m) => if (n>=m) Bool.True_ else Bool.False_))

  def fibo(n: Expression): Expression =
    Exp.oper("fibo",Array(n),t=>fibo_(t))

  private def fibo_(task: Task) = {
    val arg = task.exp.args(0)
    ret(task,
      Bool.if_then_else(
        lt(arg,value(2)),
        arg,
        plus(fibo(minus(arg,value(2))),
             fibo(minus(arg,value(1))))))
  }

  private def prim(op: Int => ConsName): Task => List[Task] = {
    task => {
      matchArg(task, 0, (int,_,_) => {
        int match {
          case Integer_(n) => retCons(task,Constructor(op(n),true),Array())
          case _ => Nil
        }
      })
    }
  }

  private def prim2(op: (Int,Int) => ConsName): Task => List[Task] = {
    task => {
      matchArgs(task,Nil,args => {
        args match {
          case (Integer_(m),_,_) :: (Integer_(n),_,_) :: Nil =>
            retCons(task,Constructor(op(m,n),true),Array())
          case _ => Nil
        }
      })
    }
  }
}  

