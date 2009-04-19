package pgep.GenotypeSelectors

class WeightedProbability(nOpParams: Int, nSelections: Int) extends GenotypeSelector {
  protected val selection = new Array[Int](nOpParams)
  protected val random = RNGProvider()
  
  override def select(genotypes: Array[Genotype]): List[List[Genotype]] = {
    val selector = new ExactSelector(genotypes map (_.matingProbability), genotypes)
    (0 until nSelections) map (_ => (0 until nOpParams) map (_ => selector()) toList) toList
  }
}
