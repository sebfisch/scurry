package scurry.rts

class Expression(var kind: ExpKind, var args: Array[Expression]) {
  def become(exp: Expression) = set(exp.kind, exp.args)

  def set(_kind: ExpKind, _args: Array[Expression]) = {
    kind = _kind
    args = _args
  }

  def isNormalised = kind match {
    case Constructor(_,isNF) => isNF
    case Failure => true
    case _ => false
  }

  def isHeadNormalised = kind match {
    case Constructor(_,_) => true
    case Failure => true
    case _ => false
  }

  def isFailure = kind match {
    case Failure => true
    case _ => false
  }

  def isOperation = kind match {
    case Operation(_,_) => true
    case _ => false
  }

  override def toString = {
    val arg_str = if (args.length == 0) "" else {
      "(" + args(0) + args.slice(1,args.length)
                          .foldLeft("")((s,x) => s + "," + x.toString) + ")"
    }
    kind.toString + arg_str
  }
}

abstract class ExpKind

abstract class ConsName

case class Constructor(name: ConsName, isNF: Boolean) extends ExpKind {
  override def toString = {
    val s = name.toString
    s.substring(0,s.length-1) //+ (if (isNF) "!" else "")
  }
}

case class Operation(name: String, code: Task => List[Task]) extends ExpKind {
  override def toString = name
}

case object Failure extends ExpKind {
  override def toString = "fail"
}

case object Choice extends ExpKind {
  override def toString = "?"
}

object Exp {
  def symb(kind: ExpKind, args: Array[Expression]) =
    new Expression(kind, args)

  def cons(name: ConsName, args: Array[Expression]) = {
    val isNF = args.forall(x => x != null && x.isNormalised)
    symb(Constructor(name,isNF), args)
  }

  def oper(name: String, args: Array[Expression], code: Task => List[Task] ) =
    symb(Operation(name,code), args)

  def failure = new Expression(Failure,Array())

  def choice(l: Expression, r: Expression) =
    new Expression(Choice,Array(l,r))
}

