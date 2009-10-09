package scurry.rts

// Scala has a funny way to specify enumeration types:
// we define a factory object with local fields for the different values...
object EvalStatus extends Enumeration {
  type EvalStatus = Value
  val 	Unused,		// not yet considered for evaluation
  		Pending, 	// needed for evaluation but not (yet) waiting
  		Waiting,	// waiting for needed subExps
  		Ready,      // about to be pending
  		HNF,		// head normalised
  		NF			// normalised
  		= Value
}

// ...and import the local fields into the current scope
import EvalStatus._

abstract class ConsName

// Exps are either choice-, Op-, or Constr-rooted. 
abstract class ExpKind
case object Choice extends ExpKind {
  override def toString = "?"
}
// Constrs only have a name
case class Constr(name: ConsName) extends ExpKind {
  override def toString = {
    val s = name.toString
    s.substring(0,s.length-1)
  }
}
// Ops additionally have code which returns a list of Exps
// that should be evaluated next. If the Exp is a redex then the code
// returns the reduct, if it has needed subExps then it returns those,
// and if it is head normalised then it returns an empty list.
case class Op(name: String, deps: Int, code: Exp => Array[Exp]) 
  extends ExpKind {
    override def toString = name
  }

class Exp(var kind: ExpKind, var args: Array[Exp]) {
  private var status: EvalStatus = Unused;
  private var parents: Array[Exp] = Array();
  
  def setKind(k: ExpKind) { kind = k }
  def setArgs(es: Array[Exp]) { args = es }
  
  def isHNF = status == NF || status == HNF
  def checkHNF = {
    val hnf = kind match {
                case Choice    => args.exists(e => e.isHNF)
                case Constr(_) => true
                case Op(_,_,_) => false
              }
    if (hnf) status = HNF
    hnf
  }
  def setHNF { status = HNF }
  
  def isNF = status == NF
  def setNF { status = NF }
  
  def isChoice = kind == Choice
  
  def isPending = status == Pending
  def setPending { status = Pending }
  def setReady { status = Ready }
  def isWaiting = status == Waiting
  def setWaiting { status = Waiting }
  
  def waitFor(exp: Exp) {
    kind match {
      case Op(name,deps,code) => kind = Op(name,deps+1,code)
      case _ => ()
    }
    status = Waiting
    exp.addParent(this)
  }
  
  // Tells whether an Exp is already scheduled or not
  def shouldBeScheduled = status == Ready || status == Unused;
  
  def addParent(exp: Exp) {
    parents = parents ++ Array(exp)
  }
  
  // Returns a list of Exps that should be evaluated next.
  // (Head) normalised Exps are not returned but returned Exps
  // may be pending already.
  def nextExps: Array[Exp] = kind match {
    case Choice => if (isHNF || checkHNF) Array()
                   else args.filter(e => !e.isHNF)
    case Op(_,_,code) => code(this)
  }
  
  // Notifies all parents that this Exp is done with evaluation.
  // Returns all parents that do not need to wait any longer.
  // The notified parents are deleted afterwards.
  def pendingParents: Array[Exp] = {
    val exps = parents.filter(e => e.mayContinue)
    parents = Array()
    exps
  }
  
  // Decrements the dependency counter and returns true if it reaches zero.
  def mayContinue: Boolean = {
    kind match {
      case Choice => true
      case Op(name,deps,code) => {
        kind = Op(name,deps-1,code)
        if (deps==1) {
          status = Ready
          true
        } else false
      }
    }
  }
  
  // replace this expression with another by copying
  def become(exp: Exp) {
    // kind,args,status,parents
    this.kind    = exp.kind
    this.args    = exp.args
    this.status  = exp.status
    this.parents = exp.parents
  }
}

// "Companion object" for creating Exp values.
object Exp {
  def choice(l: Exp, r: Exp) = {
    val exp = new Exp(Choice,Array(l,r))
    if (l.isHNF || r.isHNF) exp.setHNF
    if (l.isNF || r.isNF) exp.setNF
    exp
  }
  
  def constr(name: ConsName, args: Array[Exp]) = {
    val exp = new Exp(Constr(name),args)
    exp.setHNF
    if (args forall (e => e.isNF)) exp.setNF
    exp
  }
  
  def op(name: String, args: Array[Exp], code: Exp => Array[Exp]) = {
    new Exp(Op(name,0,code),args)
  }
}

