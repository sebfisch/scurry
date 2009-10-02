package scurry.lib

import scurry.rts._
import scurry.lib.helpers._

object Bool extends Module {

  case object True_  extends ConsName
  case object False_ extends ConsName

  def True  = Exp.cons(True_ ,Array())
  def False = Exp.cons(False_,Array())
  
  def not(arg: Expression) =
    Exp.oper("not",Array(arg),task => not_(task))

  private def not_(task: Task) = {
    matchArg(task, 0, (name,_,_) => {
      val res = name match {
        case True_ => False_
        case False_ => True_
      }
      retCons(task,Constructor(res,true),Array())
    })
  }

  def if_then_else(b: Expression, t: Expression, f: Expression) =
    Exp.oper("if_then_else",Array(b,t,f),task => if_then_else_(task))

  private def if_then_else_(task: Task) = {
    val exp = task.exp
    matchArg(task,0,(name,_,_) => {
      ret(task,name match {
        case True_  => exp.args(1)
        case False_ => exp.args(2)
      })
    })
  }

  def if_then(b: Expression, t: Expression) =
    Exp.oper("if_then",Array(b,t),task => {
      ret(task,if_then_else(b,t,Exp.failure))
    })

  def par_and(x: Expression, y: Expression) =
    Exp.oper("|&|",Array(x,y),task => par_and_(task))

  private def par_and_(task: Task) = {
    val exp = task.exp
    matchArgs(task,Nil,args => {
      retCons(
        task,
        Constructor(
          if (args.exists(x => x.equals(False_))) False_ else True_,
              true),
        Array())
    })
  }

  def eq(x: Expression, y: Expression): Expression =
    Exp.oper("==",Array(x,y),task => eq_(task))

  private def eq_(task: Task) = {
    val exp = task.exp
    matchArgs(task,Nil,args => {
      args match {
        case x :: y :: Nil =>
          if (x.equals(y)) {
            val xs = exp.args(0).args
            val ys = exp.args(1).args
            ret(task,
                xs.zip(ys)
                  .foldRight(True)((p,b) =>
                    p match {
                      case (u,v) => par_and(eq(u,v),b)
                    }))
          } else retCons(task,Constructor(False_,true),Array())
        case _ => Nil
      }
    })
  }
}  

