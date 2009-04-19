package pgep

abstract class GenotypeSelector {
  def select(genotypes: Array[Genotype]): List[List[Genotype]]
}
