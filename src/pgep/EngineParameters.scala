package pgep

object EngineParameters {
  def apply(ngenes: Int, headLen : Int, maxNrGenerations : Int, goodEnoughFitness: Double, functions: Alphabet[Func], variables: Alphabet[Var],
            tp: TermProbabilities, geneLinkingFunction: Func, operators : OperatorSet, constgen : Map[Class[_], () => Any],
            constantMutationProbability : Double, nConstants : Int, geneResultTypes : List[Class[_]]) = {
    
    assert(ngenes > 0)
    assert((geneLinkingFunction == null) != (geneResultTypes == null))
    assert(geneLinkingFunction == null || ngenes == geneLinkingFunction.nparams)
    assert(geneResultTypes == null || geneResultTypes.length == ngenes)
    
    var hlen = headLen
    var tlen = hlen * (functions.maxParams - 1) + 1
    if (functions.length == 0) {
      tlen += hlen
      hlen = 0
      assert(tlen == 1)
    }
    val grt = if (geneResultTypes != null) geneResultTypes else geneLinkingFunction.parameterTypes
    
    new EngineParameters(ngenes, hlen, tlen, maxNrGenerations, goodEnoughFitness,functions, variables, tp, geneLinkingFunction, operators, constgen,
                         constantMutationProbability, nConstants, grt)
  }
}

class EngineParameters(ngenes: Int,
                       headLen: Int,
                       tailLen: Int,
                       val maxNrGenerations: Int,
                       val goodEnoughFitness: Double,
                       functions: Alphabet[Func],
                       variables: Alphabet[Var],
                       tp: TermProbabilities,
					   geneLinkingFunction: Func,
					   val operators: OperatorSet,
					   val constgen: Map[Class[_], () => Any],
					   val constantMutationProbability: Double,
					   val nConstants: Int,
					   geneResultTypes: List[Class[_]]) {
  
  def createGenotypeParameters(constants: Map[Class[_], Alphabet[Const]]) =
    GenotypeParameters(ngenes, headLen, tailLen, geneLinkingFunction, geneResultTypes, functions, variables, constants, tp);
}
