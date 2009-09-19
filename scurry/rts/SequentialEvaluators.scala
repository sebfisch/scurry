package scurry.rts

import scurry.lib.helpers.Normalise
import scala.collection.mutable._

abstract class Q[T] {
  def isEmpty: Boolean
  def get: T
  def put(iter: Iterable[T])
}

class LifoQ[T] extends Q[T] {
  private val stack = new Stack[T]()

  def isEmpty = stack.isEmpty
  def get = stack.pop
  def put(iter: Iterable[T]) = { stack ++= iter }

  override def toString = stack.toString
}

class FifoQ[T] extends Q[T] {
  private val queue = new Queue[T]()

  def isEmpty = queue.isEmpty
  def get = queue.dequeue
  def put(iter: Iterable[T]) = { queue ++= iter }

  override def toString = queue.toString
}

class SequentialEvaluator(tasks: Q[Task]) {
  def execute(exp: Expression): Expression = {
    val goal = Normalise.nf(exp)
    tasks.put(new Task(goal, null) :: Nil)
    while (!tasks.isEmpty) {
      //println(tasks)
      val task = tasks.get
      task.perform match {
        case Nil => {
          if (task.parent != null) {
            task.parent.decDeps
            if (task.parent.deps == 0)
              tasks.put(task.parent::Nil)
          }
        }
        case newts => tasks.put(newts)
      }
    }
    goal
  }
}
