package pgep

abstract class GenotypeSelection {
	def select(genotypes: List[Genotype]): List[List[Int]]
}
