package examples

import pgep._

object Main {
  def run(engine: Engine) {
    engine.run(println(String.format("Generation: %5s\t\tbest fitness: %10.8s\t\tevaluationTime: %8.8s ms\tmutationTime: %8.8s ms",
                                     engine.generation.toString,
                                     engine.fittest.fitness.toString,
                                     engine.avgEvalTime.toString,
                                     engine.avgMutationTime.toString)),
               println(engine.fittest.toExpressionString),
               println(engine.fittest.toExpressionString))
  }
  
  def main(args: Array[String]) {
    RNGProvider.seed = 991288291
    for (engine <- List(BoolExample.create(), DoubleExample.create()))
      run(engine)
  }
}
