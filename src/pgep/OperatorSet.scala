package pgep

import GeneticOperators.Mutators.MutationBase
import GeneticOperators.Reproducers.Reproduction

object OperatorSet {
  def apply(fitnessFunction: (Genotype) => Double, mpf: MatingProbabilityFunction, gos: List[GeneticOperator]) = {
    val reproducers = gos filter {case r: Reproduction => true; case _ => false} map {case r: Reproduction => r}
    val modifiers = gos filter {case m: MutationBase => true; case _ => false} map {case m: MutationBase => m}
    assert(reproducers.length + modifiers.length == gos.length)
    new OperatorSet(fitnessFunction, mpf, reproducers, modifiers)
  }
}

class OperatorSet(val fitnessFunction: (Genotype) => Double,
                  val mpf: MatingProbabilityFunction,
                  val reproducers: List[Reproduction],
                  val modifiers: List[MutationBase])
