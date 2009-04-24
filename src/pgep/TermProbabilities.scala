package pgep

object TermProbabilities {
  def apply(functionProbs: Double, variableProbs: Double, constantProbs: Double) = {
    val scale = functionProbs + variableProbs + constantProbs
    new TermProbabilities(functionProbs / scale, variableProbs / scale, constantProbs / scale)
  }
}

class TermProbabilities private (val functionProbabilities: Double,
                                 val variableProbabilities: Double,
                                 val constantProbabilities: Double)
