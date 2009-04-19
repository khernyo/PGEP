package pgep.GeneticOperators.Reproducers

import pgep.GenotypeSelectors.Range

class CreateRandom(gtparams: GenotypeParameters, nchildren: Int) extends Reproducer(1, nchildren, new Range(1)) {
  val l = (0 until nchildren).toList
  
  protected[Reproducers] override def apply(selected: List[Genotype]) =
    l map { _ =>
      val gt = Genotype(gtparams)
      gt.randomize()
      gt
    }
}
