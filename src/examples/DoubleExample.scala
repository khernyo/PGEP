package examples

import pgep._
import pgep.GenotypeSelectors._
import pgep.GeneticOperators.Reproducers._
import pgep.GeneticOperators.Mutators._
import pgep.Functions.DoubleFunctions

object DoubleExample {
  def create() = {
    val random = RNGProvider()
    val doubleType = classOf[Double]
    
    val var_x = (-10 to 10) map (_.toDouble) toArray
    val variables = new AlphabetRO(Var('x, doubleType))
    val vcases = (for (x <- var_x) yield Map('x -> x)) toList
    val expvals =
      for (vars <- vcases;
           x = vars('x))
        yield List(x * x + 2 * x + 1)
    
    val ngenes = 3
    val headlen = 7
    val ngenerations = 500
    val nconsts = 100
    val popsize = 30
    val constantMutationProbability = 0.044
    val constantMin = -10
    val constantMax = 10
    val inversionProbability = 0.05
    val partialTranspositionProbability = 0.05
    val partialTransposonMaxLen = headlen / 4
    val inversionMaxLen = headlen / 4
    val headMutationProbability = 0.044
    val tailMutationProbability = 0.044
    val constMutationProbability = 0.044
    val maxInvalidResults = 10
    val geneLinkingFunction: Func = DoubleFunctions.add3
    val nelits = ((popsize * 0.3) max 2).toInt
    val nrandom = 5
    
    val constgen: Map[Class[_], () => Any] = Map(doubleType -> (() => random.nextDouble * (constantMax - constantMin) + constantMin))
    
    val survivors = new Range(nelits, popsize / 2 - nelits)
    val newrandoms = new Range(popsize - nrandom, nrandom)
    
    val fitnessFn: (Genotype) => Double = (_.meanSquaredError({case (expected: Double, actual: Double) => expected - actual},
                                        vcases, expvals, maxInvalidResults))
    
    val operators = OperatorSet(fitnessFn, new SogartarSemiEllipticSemiParabolic,
                                List(
                                  new Elitism(nelits, new Fittest(nelits)),
                                  new ReproductionGroup(new WeightedProbability(2, popsize / 2 - nelits),
                                                        List(0.5, 0.5),
                                                        List(new OnePointCrossover, new TwoPointCrossover)),
                                  new Mutation(headMutationProbability, tailMutationProbability, constMutationProbability, survivors),
                                  new PartialTransposition(partialTranspositionProbability, partialTransposonMaxLen, survivors),
                                  new Inversion(inversionProbability, inversionMaxLen, survivors),
                                  new Randomized(newrandoms)))
    
    val config = EngineParameters(ngenes, headlen, ngenerations, 0.0,
                                  new AlphabetRO[Func](DoubleFunctions.add, DoubleFunctions.sub, DoubleFunctions.mul, DoubleFunctions.div),
                                  variables, TermProbabilities(0.5, 0.25, 0.25),
                                  geneLinkingFunction, operators, constgen,
                                  constMutationProbability, nconsts, null)
    
    Engine(config, popsize)
  }
  
  def main(args: Array[String]): Unit = {
    Main.run(create())
  }
}
