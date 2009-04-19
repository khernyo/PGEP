package pgep.GeneticOperators.Reproducers

import pgep.GenotypeSelectors.Range

class CreateRandom(gtparams: GenotypeParameters, nchildren: Int) extends Reproducer(1, nchildren, new Range(1)) {
  protected[Reproducers] override def apply(selected: List[Genotype]) =
    (0 until nchildren).toList map { _ =>
      val gt = Genotype(gtparams)
      gt.randomize()
      gt
    }
}
