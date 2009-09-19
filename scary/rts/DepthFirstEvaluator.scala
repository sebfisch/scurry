package scary.rts

import scary.lib.Normalise

object DepthFirstEvaluator {
  private var tasks: List[Task] = _

  def execute(exp: Expression) = {
    tasks = new Task(Normalise.nf_call(exp), null) :: Nil
    var active = true
    while (active) {
      //println(tasks)
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
            case newts => newts ::: ts
          }
        }
      }
    }
  }
}
