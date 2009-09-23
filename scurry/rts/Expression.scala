package scurry.rts

class Expression(var kind: ExpKind, var args: Array[Expression]) {
  def become(exp: Expression) { set(exp.kind, exp.args) }

  def set(_kind: ExpKind, _args: Array[Expression]) {
    kind = _kind
    args = _args
  }

  def setKind(_kind: ExpKind) { kind = _kind }
  def setArgs(_args: Array[Expression]) { args = _args }

  def copy = new Expression(kind,args.toArray)

  def simplifyChoice {
    kind match {
      case Choice => {
        args.foreach(e => e.simplifyChoice)
        args = args.filter(e => !e.isFailure)
        args = args.flatMap(e => if (e.isChoice) e.args else Array(e))
        if (args.isEmpty) kind = Failure
        else if (args.length==1) become(args(0))
      }
      case _ => ()
    }
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

  def isChoice = kind match {
    case Choice => true
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
    val desc = super.toString
    kind.toString + arg_str // + desc.substring(desc.lastIndexOf('@'))
  }
}

abstract class ExpKind

abstract class ConsName

case class Constructor(name: ConsName, isNF: Boolean) extends ExpKind {
  override def toString = {
    val s = name.toString
    s.substring(0,s.length-1) + (if (isNF) "!" else "")
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

  def bin_choice(l: Expression, r: Expression) = choice(Array(l,r))
  def choice(args: Array[Expression]) = new Expression(Choice,args)
  def simple_choice(args: Array[Expression]) = {
    val c = choice(args)
    c.simplifyChoice
    c
  }
}

