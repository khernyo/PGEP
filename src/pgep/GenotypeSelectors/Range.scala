package pgep.GenotypeSelectors

class Range(start: Int, count: Int) extends GenotypeSelector {
  def this(count: Int) = this(0, count)
  
  protected val selection = (start until (start + count)) map (i => List(i)) toList
  
  override def select(genotypes: Array[Genotype]): List[List[Int]] = selection 
}
