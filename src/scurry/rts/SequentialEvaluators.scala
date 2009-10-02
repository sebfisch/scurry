package scurry.rts

import scurry.lib.helpers._
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

class SequentialEvaluator(tasks: Q[Task]) extends ExpIterator {
  private var interrupted: Boolean = false
  private var nextExp: Expression = null

  private var goal: Expression = null

  def init(exp: Expression) {
    goal = Normalise.hnf(this,Normalise.nf(exp))
    tasks.put(new Task(goal,null) :: Nil)    
  }

  def putNext(exp: Expression) {
    interrupted = true
    nextExp = exp
  }

  def hasNext = nextExp != null || computeNext

  def next = if (hasNext) { 
    val res = nextExp
    interrupted = false
    nextExp = null
    res
  } else throw new NoSuchElementException()

  private def computeNext: Boolean = {
    while (!(interrupted || tasks.isEmpty)) {
//       println(goal)
//       println(" " + tasks)
      val task = tasks.get
      task.perform match {
        case Nil => {
          if (task.parent != null) {
            task.parent.decDeps
            if (task.parent.deps.isEmpty || task.parent.deps.get == 0)
              tasks.put(task.parent::Nil)
          }
        }
        case newts => tasks.put(newts)
      }
    }
    !(nextExp == null && tasks.isEmpty)
  }
}
