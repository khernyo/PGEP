package pgep

abstract class GeneticOperator(val nparams: Int,
                               val nchildren: Int,
                               val selection: GenotypeSelection) {
  protected val random = RNGProvider() 
}
