package scary

class Task(_exp: Expression, _parent: Task) {
  private var _deps = 0

  def exp = _exp
  def parent = _parent

  def deps = _deps
  def setDeps(count: Int) { _deps = count }
  def decDeps { _deps = _deps - 1 }

  def perform = {
    exp.kind match {
      case Constructor(_,_) => Nil
      case Operation(_, code) => code(this)
    }
  }

  override def toString = exp.toString
}
