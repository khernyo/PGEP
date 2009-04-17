package pgep

import scala.collection.Map

object GeneParameters {
  protected def check(gp: GeneParameters) {  
    assume(gp.constants forall {case (t, a) => a forall (_.typee == t)}, "inconsistent constants")
    
    val types = new scala.collection.mutable.HashSet[Class[_]]
    types ++= gp.functions flatMap (f => f.resultType :: f.parameterTypes)
    types ++= gp.variables map (_.typee)
    types ++= gp.constants.keys
    
    for (t <- types) {
      if (!gp.functions.exists(_.resultType == t) ||
          !gp.variables.exists(_.typee == t) ||
          !gp.constants.exists(_ == t))
        throw new IllegalArgumentException("Not all types are covered by Alphabets!");
    }  
  }
}

class GeneParameters(val headLen: Int,
                     val tailLen: Int,
                     val resultType: Class[_],
                     val functions: Alphabet[Func],
                     val variables: Alphabet[Var],
                     val constants: Map[Class[_], Alphabet[Const]]) {

  GeneParameters.check(this)
  
  def geneLen = headLen + 2 * tailLen
} 