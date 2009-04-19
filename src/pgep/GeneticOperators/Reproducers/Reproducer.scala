package pgep.GeneticOperators.Reproducers

abstract class Reproducer(nparams: Int, nchildren: Int, selection: GenotypeSelector) extends GeneticOperator(nparams, nchildren, selection) {
  def apply(src: Array[Genotype]): List[Genotype] =
    selection.select(src) flatMap {sel =>
      assert(sel.length == nparams)
      apply(sel)
    }
  
  protected[Reproducers] def apply(selected: List[Genotype]): List[Genotype]
}
