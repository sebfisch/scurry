package scurry.rts

class Task(var _exp: Expression, var _parent: Task) {
  private var _deps: Option[Int] = None

  def exp = _exp
  def setExp(e: Expression) { _exp = e }

  def parent = _parent

  def deps = _deps
  def setDeps(count: Int) {
    if (count > 0) _deps = Some(count)
  }
  def clearDeps { _deps = None }
  def decDeps { _deps = _deps.map(c => c-1) }

  private case object CANCELED_ extends ConsName

  def cancel {
    _exp = Exp.cons(CANCELED_,Array())
    _parent = null
  }

  def perform = {
    exp.kind match {
      case Operation(_,code) => code(this)
      case _ => Nil
    }
  }

  def copy = {
    var t = new Task(exp.copy,parent)
    t._deps = _deps
    t
  }

  override def toString = {
    val p_str = if (parent==null) "" else "<" + parent.exp.kind + ">"
    exp.toString + p_str
  }
}
