package pgep

abstract class GenotypeSelection {
	def select(genotypes: Array[Genotype]): List[List[Int]]
}
