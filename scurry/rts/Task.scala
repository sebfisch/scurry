package scurry.rts

class Task(var _exp: Expression, _parent: Task) {
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

  def perform = {
//     println("permorming task for: " + this)
    exp.kind match {
      case Constructor(_,_) => Nil
      case Operation(_, code) => code(this)
      case Choice => Nil
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
