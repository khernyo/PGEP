package pgep.GenotypeSelectors

class Range(start: Int, count: Int) extends GenotypeSelector {
  def this(count: Int) = this(0, count)
  
  override def select(genotypes: Array[Genotype]): List[List[Genotype]] = genotypes.drop(start).take(count) map (List(_)) toList
}
