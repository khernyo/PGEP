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
    
    val config = EngineParameters(ngenes, headlen, ngenerations, 0.0,
                                  new AlphabetRO[Func](BoolFunctions.and, BoolFunctions.or, BoolFunctions.not),
                                  variables, TermProbabilities(0.5, 0.25, 0.25),
                                  geneLinkingFunction, operators, constgen,
                                  constMutationProbability, nconsts, List(boolType))
    
    Engine(config, popsize)
  }
  
  def main(args: Array[String]): Unit = {
    val engine = create()
    
    val startTime = new java.util.Date
    engine.run(println(String.format("Generation: %5s\t\tbest fitness: %10.5s\t\tevaluationTime: %8.8s ms\tmutationTime: %8.8s ms",
                                     engine.generation.toString,
                                     engine.fittest.fitness.toString,
                                     engine.avgEvalTime.toString,
                                     engine.avgMutationTime.toString)),
               println(engine.fittest.toExpressionString),
               println(engine.fittest.toExpressionString))
    val endTime = new java.util.Date
    println(endTime.getTime - startTime.getTime)
  }
}
