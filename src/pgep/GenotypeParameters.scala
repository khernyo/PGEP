package pgep

class GenotypeParameters(val nrGenes: Int, val geneLinkingFunction: Func,
                         val selector_fvc: Selector[Term], val selector_vc: Selector[Term],
						 val geneParameters: Seq[GeneParameters]) {
  
  def this(nrGenes: Int, headLen: Int, tailLen: Int, geneLinkingFunction: Func, geneResultTypes: List[Class[_]],
		   functions: Alphabet[Func], variables: Alphabet[Var], constants: Map[Class[_], Alphabet[Const]],
		   tp: TermProbabilities) {
	this(nrGenes, geneLinkingFunction, 
      new ExactSelector[Term](List(tp.functionProbabilities, tp.variableProbabilities, tp.constantProbabilities),
    						  List(FuncKind(), VarKind(), ConstKind())),
      new ExactSelector[Term](List(tp.variableProbabilities, tp.constantProbabilities),
                              List(VarKind(), ConstKind())),
      (0 until nrGenes) map (i => new GeneParameters(headLen, tailLen, geneResultTypes(i), functions, variables, constants)))
  }
  
  def geneLen = geneParameters(0).geneLen
  def headLen = geneParameters(0).headLen
  def tailLen = geneParameters(0).tailLen
}
