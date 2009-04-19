package pgep.GenotypeSelectors

class Fittest(selectionSize: Int) extends GenotypeSelector {
  override def select(genotypes: Array[Genotype]): List[List[Genotype]] =
    List(genotypes.toList sort (_.fitness < _.fitness) take selectionSize)
}
