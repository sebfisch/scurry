package scary

object DepthFirstEvaluator {
  private var tasks: List[Task] = _

  def hnf(exp: Expression) = {
    tasks = new Task(exp, null) :: Nil
    var active = true
    while (active) {
      tasks match {
        case Nil => active = false
        case task :: ts => {
          tasks = task.perform match {
            case Nil => {
              if (task.parent != null) {
                task.parent.decDeps
                if (task.parent.deps == 0)
                  task.parent :: ts
                else ts
              } else ts
            }
            case newts => newts ::: tasks
          }
        }
      }
    }
  }
}
