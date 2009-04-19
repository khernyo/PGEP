package pgep.GeneticOperators.Reproducers

class Elitism(nChildren: Int, selection: GenotypeSelector) extends Reproducer(nChildren, nChildren, selection) {
  protected[Reproducers] override def apply(selected: List[Genotype], dst: Array[Genotype], dstIdx: Int) = {
    var di = dstIdx
    for (gt <- selected) {
      dst(di) = gt.clone()
      di += 1
    }
    di
  }
}
