package pgep

import scala.collection.Map

object GenotypeParameters {
  
  def apply(nrGenes: Int, headLen: Int, tailLen: Int, geneLinkingFunction: Func, geneResultTypes: List[Class[_]],
		   functions: Map[Class[_], Alphabet[Func]], variables: Map[Class[_], Alphabet[Var]], constants: Map[Class[_], Alphabet[Const]],
		   tp: TermProbabilities) = {
    val selector_fvc = new ExactSelector[Term](List(tp.functionProbabilities, tp.variableProbabilities, tp.constantProbabilities),
    						  List(FuncKind(), VarKind(), ConstKind()))
    val selector_vc = new ExactSelector[Term](List(tp.variableProbabilities, tp.constantProbabilities),
                              List(VarKind(), ConstKind()))
    val geneParams = (0 until nrGenes) map (i => new GeneParameters(headLen, tailLen, geneResultTypes(i), functions, variables, constants))
    
	new GenotypeParameters(nrGenes, geneLinkingFunction, selector_fvc, selector_vc, geneParams)
  }
}

class GenotypeParameters private (val nrGenes: Int,
                                  val geneLinkingFunction: Func,
                                  val selector_fvc: Selector[Term],
                                  val selector_vc: Selector[Term],
                                  val geneParameters: Seq[GeneParameters]) {
  
  def geneLen = geneParameters(0).geneLen
  def headLen = geneParameters(0).headLen
  def tailLen = geneParameters(0).tailLen
}
