package pgep

import scala.collection.Map

object GeneParameters {
  protected def check(gp: GeneParameters) {  
    require(gp.functions forall {case (t, a) => a forall (_.resultType == t)}, "inconsistent functions")
    require(gp.variables forall {case (t, a) => a forall (_.typ == t)}, "inconsistent variables")
    require(gp.constants forall {case (t, a) => a forall (_.typ == t)}, "inconsistent constants")
    
    val resultTypes = Set.empty ++ (gp.functions.keys ++ gp.variables.keys ++ gp.constants.keys)
    val parameterTypes = Set.empty ++ (for (fns <- gp.functions.values.toList;
                                  			fn <- fns;
                                  			t <- fn.parameterTypes)
                               			 yield t)
    
    for (t <- resultTypes ++ parameterTypes) {
      if (!gp.functions.keys.exists(_ == t) ||
          !gp.variables.keys.exists(_ == t) ||
          !gp.constants.keys.exists(_ == t))
        throw new IllegalArgumentException("Not all types are covered by Alphabets!");
    }  
  }
}

class GeneParameters(val headLen: Int,
                     val tailLen: Int,
                     val resultType: Class[_],
                     val functions: Map[Class[_], Alphabet[Func]],
                     val variables: Map[Class[_], Alphabet[Var]],
                     val constants: Map[Class[_], Alphabet[Const]]) {

  GeneParameters.check(this)
  
  def geneLen = headLen + 2 * tailLen
} 