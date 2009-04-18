package pgep.GenotypeSelectors

class Fittest(selectionSize: Int) extends GenotypeSelector {
  override def select(genotypes: Array[Genotype]): List[List[Int]] = {
    List(genotypes.toList.zipWithIndex sort {
        case ((gt1, _), (gt2, _)) => gt1.fitness < gt2.fitness
      } take selectionSize map {case (gt, i) => i}) 
  }
}
