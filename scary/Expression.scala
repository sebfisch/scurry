package scary

class Expression(var kind: ExpKind, var args: Array[Expression]) {
  def set(_kind: ExpKind, _args: Array[Expression]) = {
    kind = _kind
    args = _args
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

abstract class Symbol(name: String) extends ExpKind {
  override def toString = name
}

case class Constructor(
  name: String,
  normalised: Boolean
 ) extends Symbol(name)

case class Operation(
  name: String,
  code: Task => List[Task]
 ) extends Symbol(name)

case object Choice extends ExpKind {
  override def toString = "?"
}

object Exp {
  def symb(kind: ExpKind, args: Array[Expression]) =
    new Expression(kind, args)

  def cons(name: String, args: Array[Expression]) =
    symb(Constructor(name,false), args)

  def oper(name: String, args: Array[Expression], code: Task => List[Task] ) =
    symb(Operation(name,code), args)
}
