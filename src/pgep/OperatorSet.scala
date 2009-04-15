package pgep

class OperatorSet(val fitnessFunction: (Genotype) => Double, val mpf: MatingProbabilityFunction, gos: List[GeneticOperator]) {
  val reproducers = gos filter {case r: Reproduction => true; case _ => false}
  val modifiers = gos filter {case m: MutationBase => true; case _ => false}
  
  {
    assert(reproducers.length + modifiers.length == gos.length)
  }
}
