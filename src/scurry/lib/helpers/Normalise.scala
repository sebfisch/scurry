package scurry.lib.helpers

import scurry.rts._

abstract class ExpIterator extends Iterator[Exp] {
  def putNext(exp: Exp)
}

object Normalise extends Module {
  def hnf(master: ExpIterator, exp: Exp) =
    Exp.op("hnf",Array(exp),e=>hnf_(master,e))

  private def hnf_(master: ExpIterator, exp: Exp): Array[Exp] = {
    matchArg(exp,0,(_,_) => { 
      master.putNext(exp.args(0))
      Array()
    })
  }

  def nf(exp: Exp) = Exp.op("nf",Array(exp),nf_)

  private def nf_(exp: Exp): Array[Exp] = {
    if (exp.isNF) {
      exp.become(exp.args(0))
      Array()
    } else {
      matchArg(exp,0,(name,args) => {
        val consexp = exp.args(0)
        consexp.setKind(Op(name.toString, 0, e=>nf_cons_(name,e)))
        consexp.setArgs(args.map(x => nf(x)))
        Array(consexp)
      })
    }
  }

  private def nf_cons_(name: ConsName, exp: Exp) = {
    matchArgs(exp,Array(),_ => ret(exp,Exp.constr(name,exp.args)))
  }
}
