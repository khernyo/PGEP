package examples

import pgep._
import pgep.GenotypeSelectors._
import pgep.GeneticOperators.Reproducers._
import pgep.GeneticOperators.Mutators._

object BoolExample {
  def run() {
    val random = RNGProvider()
    val boolType = classOf[Boolean]
    
    val var_x = Array(false, true)
    val var_y = Array(false, true)
    val variables = new AlphabetRO(Var('x, boolType), Var('y, boolType))
    val vcases = (for (x <- var_x; y <- var_y) yield Map('x -> x, 'y -> y)) toList
    val expvals = for (vars <- vcases) yield List(vars('x) || vars('y))
    
    val ngenes = 1
    val headlen = 7
    val ngenerations = 500
    val nconsts = 100
    val popsize = 30
    val constantMutationProbability = 0.044
    val inversionProbability = 0.05
    val partialTranspositionProbability = 0.05
    val partialTransposonMaxLen = headlen / 4
    val inversionMaxLen = headlen / 4
    val headMutationProbability = 0.044
    val tailMutationProbability = 0.044
    val constMutationProbability = 0.044
    val maxInvalidResults = 10
    val geneLinkingFunction: Func = null
    val nelits = 1
    val nrandom = 5
    
    val constgen: Map[Class[_], () => Any] = Map(boolType -> (() => if (random.nextDouble < 0.5) true else false))
    
    val survivors = new Range(nelits, popsize / 2 - nelits)
    val newrandoms = new Range(popsize - nrandom, nrandom)
    
    val fitnessFn: (Genotype) => Double = (_.meanSquaredError({case (expected: Boolean, actual: Boolean) => if (expected == actual) 0 else 1},
                                        vcases, expvals, maxInvalidResults))
    
    val operators = OperatorSet(fitnessFn, new SogartarSemiEllipticSemiParabolic,
                                List(
                                  new Elitism(nelits, new Fittest(nelits)),
                                  new ReproductionGroup(new WeightedProbability(2, popsize / 2 - nelits),
                                                        List(0.5, 0.5),
                                                        List(new OnePointCrossover(null), new TwoPointCrossover(null))),
                                  new Mutation(headMutationProbability, tailMutationProbability, constMutationProbability, survivors),
                                  new PartialTransposition(partialTranspositionProbability, partialTransposonMaxLen, survivors),
                                  new Inversion(inversionProbability, inversionMaxLen, survivors),
                                  new Randomized(newrandoms)
                                  ))
    
    val config = EngineParameters(ngenes, headlen, ngenerations,
                                  new AlphabetRO[Func](),
                                  variables, TermProbabilities(0.5, 0.25, 0.25),
                                  geneLinkingFunction, operators, constgen,
                                  constMutationProbability, nconsts, List(boolType))
    
    val engine = Engine(config, popsize)
    
    engine
  }
  
  def main(args: Array[String]): Unit = {
    run()
  }
}
