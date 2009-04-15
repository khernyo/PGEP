package pgep

abstract class Reproduction(nparams: Int, nchildren: Int, selection: GenotypeSelection) extends GeneticOperator(nparams, nchildren, selection) {
  def apply(src: List[Genotype], dst: Array[Genotype], dstIdx: Int): Int = {
    var di = dstIdx
    for (sel <- selection.select(src)) {
      assert(sel.length == nparams)
      di = apply(src, sel, dst, di)
    }
    
    di
  }
  
  protected def apply(src: List[Genotype], selectionIndices: List[Int], dst: Array[Genotype], dstIdx: Int): Int
}
