package scurry.rts

import scurry.lib.helpers._
import scala.collection.mutable._

abstract class Q[T] {
  def isEmpty: Boolean
  def get: T
  def put(elem: T)
}

class LifoQ[T] extends Q[T] {
  private val stack = new Stack[T]()

  def isEmpty = stack isEmpty
  def get = stack pop
  def put(elem: T) = { stack push elem }

  override def toString = stack toString
}

class FifoQ[T] extends Q[T] {
  private val queue = new Queue[T]()

  def isEmpty = queue isEmpty
  def get = queue dequeue
  def put(elem: T) = { queue enqueue elem }

  override def toString = queue toString
}

class SequentialEvaluator(pending: Q[Expression]) extends ExpIterator {
  private var interrupted: Boolean = false
  private var nextExp: Expression = null

  private var goal: Expression = null

  def init(exp: Expression) {
    goal = Normalise.hnf(this,Normalise.nf(exp))
    pending.put(goal)    
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
  
  private def addPending(exps: Iterable[Expression]) {
	exps.foreach(e => if (e.shouldBeScheduled) {
                		e.setPending
                		pending.put(e)
              		  } else ())
  }

  private def computeNext: Boolean = {
    while (!(interrupted || pending.isEmpty)) {
      val exp = pending.get
      var exps = exp.nextExps
      if (exps.isEmpty) addPending(exp.pendingParents)
      else addPending(exps)
    }
    !(nextExp == null && pending.isEmpty)
  }
}
