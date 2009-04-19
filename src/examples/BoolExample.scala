package examples

import pgep._
import pgep.GenotypeSelectors._
import pgep.GeneticOperators.Reproducers._
import pgep.GeneticOperators.Mutators._
import pgep.Functions.BoolFunctions

object BoolExample {
  def create() = {
    val random = RNGProvider()
    val boolType = classOf[Boolean]
    
    val var_x = Array(false, true)
    val var_y = Array(false, true)
    val variables = List(Var('x, boolType), Var('y, boolType))
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
    
    val nelits = 2
    val nmutatedelits = (popsize * 0.1).toInt
    val nsurvivor = (popsize * 0.7).toInt - nmutatedelits
    val nrandom = 5
    val mutationrange = new Range(nelits, nmutatedelits + nsurvivor)
    
    val constgen: Map[Class[_], () => Any] = Map(boolType -> (() => if (random.nextDouble < 0.5) true else false))
        
    val fitnessFn: (Genotype) => Double = (_.meanSquaredError({case (expected: Boolean, actual: Boolean) => if (expected == actual) 0 else 1},
                                        vcases, expvals, maxInvalidResults))
    
    val operators = OperatorSet(fitnessFn, new SogartarSemiEllipticSemiParabolic,
                                List(
                                  new Clone(nelits, new Fittest(nelits)),
                                  new Clone(nmutatedelits, new Fittest(nmutatedelits)),
                                  new ReproductionGroup(new WeightedProbability(2, nsurvivor),
                                                        List(0.5, 0.5),
                                                        List(new OnePointCrossover, new TwoPointCrossover)),
                                  new Mutation(headMutationProbability, tailMutationProbability, constMutationProbability, mutationrange),
                                  new PartialTransposition(partialTranspositionProbability, partialTransposonMaxLen, mutationrange),
                                  new Inversion(inversionProbability, inversionMaxLen, mutationrange)))
    
    val config = EngineParameters(popsize, ngenes, headlen, ngenerations, 0.0,
                                  List(BoolFunctions.and, BoolFunctions.or, BoolFunctions.not),
                                  variables, TermProbabilities(0.5, 0.25, 0.25),
                                  geneLinkingFunction, operators, constgen,
                                  constMutationProbability, nconsts, List(boolType))
    
    Engine(config)
  }
  
  def main(args: Array[String]): Unit = {
    Main.run(create())
  }
}
