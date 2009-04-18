package pgep.GeneticOperators.Reproducers

class ReproductionGroup(selection: GenotypeSelector,
                        probabilities: List[Double],
                        ops: List[Reproducer]) extends Reproducer(ops.head.nparams, ops.head.nchildren, selection) {
  require(ops forall (op => op.nchildren == nchildren && op.nparams == nparams))

  protected val selector = new ExactSelector(probabilities, ops)
  
  protected[Reproducers] override def apply(src: Array[Genotype], selectionIndices: List[Int], dst: Array[Genotype], dstIdx: Int): Int =
    selector()(src, selectionIndices, dst, dstIdx)
}
