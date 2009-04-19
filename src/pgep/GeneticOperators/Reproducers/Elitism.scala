package pgep.GeneticOperators.Reproducers

class Elitism(nChildren: Int, selection: GenotypeSelector) extends Reproducer(nChildren, nChildren, selection) {
  protected[Reproducers] override def apply(src: Array[Genotype], selectionIndices: List[Int], dst: Array[Genotype], dstIdx: Int) = {
    var di = dstIdx
    for (i <- selectionIndices) {
      dst(di) = src(i).clone()
      di += 1
    }
    di
  }
}
