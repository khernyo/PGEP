package examples

import pgep._

object Main {
  def run(engine: Engine) {
    engine.run(println(String.format("Generation: %5d\t\tbest fitness: %10.10f\t\tevaluationTime: %8.8f ms\tmutationTime: %8.8f ms",
                                     int2Integer(engine.generation),
                                     double2Double(engine.fittest.fitness),
                                     double2Double(engine.avgEvalTime),
                                     double2Double(engine.avgMutationTime))),
               println(engine.fittest.toExpressionString),
               println(engine.fittest.toExpressionString))
  }
  
  def main(args: Array[String]) {
    for (engine <- List(BoolExample.create(), DoubleExample.create()))
      run(engine)
  }
}
