package pgep

import scala.collection.Map

class GeneParameters(val headLen: Int, val tailLen: Int, val resultType: Class[_],
                     val functions: Alphabet[Func], val variables: Alphabet[Var], val constants: Map[Class[_], Alphabet[Const]]) {
  {  
    assume(constants forall {case (t, a) => a forall (_.typee == t)}, "inconsistent constants")
    
    val types = new scala.collection.mutable.HashSet[Class[_]]
    types ++= functions flatMap (f => f.resultType :: f.parameterTypes)
    types ++= variables map (_.typee)
    types ++= constants.keys
    
    for (t <- types) {
      if (!functions.exists(_.resultType == t) ||
          !variables.exists(_.typee == t) ||
          !constants.exists(_ == t))
        throw new IllegalArgumentException("Not all types are covered by Alphabets!");
    }
  }
  
  def geneLen = headLen + 2 * tailLen
} 