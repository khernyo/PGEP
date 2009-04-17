package pgep.GeneticOperators

class Mutation(headMutProb: Double, tailMutProb: Double, constMutProb: Double, selection: GenotypeSelection) extends MutationBase(selection) {
  require(headMutProb >= 0)
  require(tailMutProb >= 0)
  require(constMutProb >= 0)
  
  protected override def apply(gt: Genotype) {
    gt.randomize(() => random.nextDouble() < headMutProb,
    			 () => random.nextDouble() < tailMutProb,
    			 () => random.nextDouble() < constMutProb)
  }
}
