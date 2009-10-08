package scurry.rts

// Scala has a funny way to specify enumeration types:
// we define a factory object with local fields for the different values...
object EvalStatus extends Enumeration {
  type EvalStatus = Value
  val 	Unused,		// not yet considered for evaluation
  		Pending, 	// needed for evaluation but not (yet) waiting
  		Waiting,	// waiting for needed subexpressions
  		HNF,		// head normalised
  		NF			// normalised
  		= Value
}

// ...and import the local fields into the current scope
import EvalStatus._

// Expressions are either choice-, operation-, or constructor-rooted. 
abstract class ExpKind
case object Choice extends ExpKind
// Constructors only have a name
case class Constructor(name: String) extends ExpKind
// Operations additionally have code which returns a list of expressions
// that should be evaluated next. If the expression is a redex then the code
// returns the reduct, if it has needed subexpressions then it returns those,
// and if it is head normalised then it returns an empty list.
case class Operation(name: String, deps: Int, code: () => Array[Expression]) 
  extends ExpKind

class Expression(var kind: ExpKind, var args: Array[Expression]) {
  private var status: EvalStatus = Unused;
  private var parents: Array[Expression] = Array();
  
  def isHNF = status == NF || status == HNF
  def checkHNF = {
    val hnf = kind match {
                case Choice           => args.exists(e => e.isHNF)
                case Constructor(_)   => true
                case Operation(_,_,_) => false
              }
    if (hnf) status = HNF
    hnf
  }
  
  def setPending = status = Pending
  
  // Tells whether an expression is already scheduled or not
  def shouldBeScheduled = status == Waiting || status == Unused;
  
  // Returns a list of expressions that should be evaluated next.
  // (Head) normalised expressions are not returned but returned expressions
  // may be pending already.
  def nextExps: Array[Expression] = kind match {
    case Choice => if (isHNF || checkHNF) Array()
                   else args.filter(e => !e.isHNF)
    case Operation(_,_,code) => code()
  }
  
  def pendingParents: Array[Expression] = {
    val exps = parents.filter(e => e.mayContinue)
    parents = Array()
    exps
  }
  
  def mayContinue: Boolean = {
    kind match {
      case Choice => true
      case Operation(name,deps,code) => {
        kind = Operation(name,deps-1,code)
        deps==1
      }
    }
  }
}

