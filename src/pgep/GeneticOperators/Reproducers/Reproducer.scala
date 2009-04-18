package pgep.GeneticOperators.Reproducers

abstract class Reproducer(nparams: Int, nchildren: Int, selection: GenotypeSelector) extends GeneticOperator(nparams, nchildren, selection) {
  def apply(src: Array[Genotype], dst: Array[Genotype], dstIdx: Int): Int = {
    var di = dstIdx
    for (sel <- selection.select(src)) {
      assert(sel.length == nparams)
      di = apply(src, sel, dst, di)
    }
    
    di
  }
  
  protected[Reproducers] def apply(src: Array[Genotype], selectionIndices: List[Int], dst: Array[Genotype], dstIdx: Int): Int
}
