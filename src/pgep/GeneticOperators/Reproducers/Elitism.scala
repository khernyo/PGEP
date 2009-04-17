package pgep.GeneticOperators.Reproducers

class Elitism(nChildren: Int, selection: GenotypeSelection) extends Reproduction(nChildren, nChildren, selection) {
  protected[Reproducers] override def apply(src: Array[Genotype], selectionIndices: List[Int], dst: Array[Genotype], dstIdx: Int) = {
    var di = dstIdx
    for (i <- (0 until selectionIndices.length)) {
      dst(di) = src(selectionIndices(i)).clone()
      di += 1
    }
    di
  }
}
