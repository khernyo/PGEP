package pgep

import GeneticOperators.Mutators.Mutator
import GeneticOperators.Reproducers.Reproducer

object OperatorSet {
  def apply(fitnessFunction: (Genotype) => Double, mpf: MatingProbabilityFunction, gos: List[GeneticOperator]) = {
    val reproducers = gos filter {case r: Reproducer => true; case _ => false} map {case r: Reproducer => r}
    val modifiers = gos filter {case m: Mutator => true; case _ => false} map {case m: Mutator => m}
    assert(reproducers.length + modifiers.length == gos.length)
    new OperatorSet(fitnessFunction, mpf, reproducers, modifiers)
  }
}

class OperatorSet(val fitnessFunction: (Genotype) => Double,
                  val mpf: MatingProbabilityFunction,
                  val reproducers: List[Reproducer],
                  val modifiers: List[Mutator])
