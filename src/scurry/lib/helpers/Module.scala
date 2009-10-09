package scurry.lib.helpers

import scurry.rts._

class Module {
  def ret(exp: Exp, result: Exp) = {
    exp.become(result)
    retNext(result)
  }

  private def retNext(exp: Exp): Array[Exp] = {
    exp.kind match {
      case Op(_,_,_) => Array(exp)
      case _ => Array()
    }
  }

  def matchArg(exp: Exp, index: Int, 
               replace: (ConsName,Array[Exp]) => Array[Exp]) = {
    val arg = exp.args(index)
    if (arg.isHNF) {
      arg.kind match {
        case Choice => distribute(exp,index)
        case Constr(name) => replace(name,arg.args)
      }
    } else { // operation-rooted or non-hnf choice
      exp.waitFor(arg)
      Array(arg)
    }
  }
  
  private def distribute(exp: Exp, index: Int) = {
    val arg = exp.args(index)
    
    var exps = arg.args.map(e => {
                 // copy distributed node
                 val es = exp.args.toArray
                 // but update the corresponding argument
                 es.update(index,e)
                 val alt = new Exp(exp.kind,es)
                 // add choice as parent
                 alt.addParent(exp)
                 // wait for active alternatives and continue with others
                 if (e.isPending || e.isWaiting) alt.waitFor(e)
                 else alt.setReady
                 alt
               })

    exp.setKind(Choice/* == arg.kind*/)
    exp.setWaiting
    exp.setArgs(exps)
    
    exps
  }

  def matchArgs(exp: Exp, indices: Array[Int], 
                replace: List[ConsName] => Array[Exp]) =
  {
    val args = if (indices.isEmpty) exp.args else indices.map(exp.args)
    
    if (args.forall(_.isHNF)) {
      val choiceIndex = args.findIndexOf(_.isChoice)
      if (choiceIndex >= 0) {
        matchArg(exp,choiceIndex, (_,_) => { exp.setReady; Array(exp) })
      } else {
        replace(args.map(e => e.kind match {
          case Constr(name) => name
        }).toList)
      }
    } else {
      val exps = args.filter(!_.isHNF)
      exps.foreach(exp.waitFor(_))
      exps
    }
  }
}
