package pgep

class OperatorSet(val fitnessFunction: (Genotype) => Double, val mpf: MatingProbabilityFunction, gos: List[GeneticOperator]) {
  val reproducers = gos filter {case r: Reproduction => true; case _ => false} map {case r: Reproduction => r}
  val modifiers = gos filter {case m: MutationBase => true; case _ => false} map {case m: MutationBase => m}
  
  {
    assert(reproducers.length + modifiers.length == gos.length)
  }
}
