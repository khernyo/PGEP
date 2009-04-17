package pgep.GeneticOperators.Reproducers

class ReproductionGroup(selection: GenotypeSelection,
                        probabilities: List[Double],
                        ops: List[Reproduction]) extends Reproduction(ops.head.nparams, ops.head.nchildren, selection) {
  require(ops forall (op => op.nchildren == nchildren && op.nparams == nparams))

  protected val selector = new ExactSelector(probabilities, ops)
  
  protected[Reproducers] override def apply(src: Array[Genotype], selectionIndices: List[Int], dst: Array[Genotype], dstIdx: Int): Int =
    selector.apply().apply(src, selectionIndices, dst, dstIdx)
}
