package pgep

import scala.collection.Map

class EngineParameters(ngenes: Int, headLen_ : Int, maxNrGenerations_ : Int,
                       functions: Alphabet[Func], variables: Alphabet[Var], tp: TermProbabilities,
					   geneLinkingFunction: Func, operators_ : OperatorSet, constCreators_ : Map[Class[_], () => Any],
					   constantMutationProbability_ : Double, nConstants_ : Int, geneResultTypes_ : List[Class[_]]) {
  val operators = operators_
  val constCreators = constCreators_
  val nConstants = nConstants_
  val constantMutationProbability = constantMutationProbability_
  val maxNrGenerations = maxNrGenerations_
  
  protected val (headLen, tailLen) = {
      if (functions.length == 0)
        (0, headLen_ * (functions.maxParams - 1) + 1 + headLen_)
      else
        (headLen_, headLen_ * (functions.maxParams - 1) + 1)
    }
  protected val geneResultTypes = if (geneResultTypes_ != null) geneResultTypes_ else geneLinkingFunction.parameterTypes
  
  def createGenotypeParameters(constants: Map[Class[_], Alphabet[Const]]) = {
    new GenotypeParameters(ngenes, headLen, tailLen, geneLinkingFunction, geneResultTypes, functions, variables, constants, tp)
  }
  
  {
    assert(ngenes > 0)
    assert((geneLinkingFunction == null) != (geneResultTypes_ == null))
    assert(geneLinkingFunction == null || ngenes == geneLinkingFunction.nparams)
    assert(geneResultTypes_ == null || geneResultTypes_.length == ngenes)  

    if (functions.length == 0)
      assert(tailLen == 1)
  }
}
