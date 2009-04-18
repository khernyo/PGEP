package pgep.GeneticOperators.Mutators

class Mutation(headMutProb: Double, tailMutProb: Double, constMutProb: Double, selection: GenotypeSelector) extends Mutator(selection) {
  require(headMutProb >= 0)
  require(tailMutProb >= 0)
  require(constMutProb >= 0)
  
  protected[Mutators] override def apply(gt: Genotype) {
    gt.randomize(() => random.nextDouble() < headMutProb,
    			 () => random.nextDouble() < tailMutProb,
    			 () => random.nextDouble() < constMutProb)
  }
}
