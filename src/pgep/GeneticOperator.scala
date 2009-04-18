package pgep

abstract class GeneticOperator(val nparams: Int,
                               val nchildren: Int,
                               val selection: GenotypeSelector) {
  protected val random = RNGProvider() 
}
