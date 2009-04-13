package pgep

import scala.collection.mutable.{HashMap,HashSet}

class Gene(val parameters: GeneParameters) {
  val _k_expression = new HashMap[Class[_], Array[Term]]
  private val _constants = new HashMap[Class[_], Array[Const]]
  
  private val random = RNGProvider()
  
  {
    val types = new HashSet[Class[_]]
    types ++= parameters.functions map (_.resultType)
    types ++= parameters.variables map (_.typee)
    types ++= parameters.constants.keys
    
    types foreach { t =>
      _k_expression(t) = new Array(parameters.headLen + parameters.tailLen)
      _constants(t) = new Array(parameters.tailLen)
    }
  }
  
  def randomize(selector_fvc: Selector[SymbolSetKind], selector_vc: Selector[SymbolSetKind],
  				headp: () => Boolean, tailp: () => Boolean, constantsp: () => Boolean) {
    for (tpe <- _k_expression.keys) {
      val k_expr = _k_expression(tpy)
      val functions = parameters.functions filter (_.resultType == tpe) toArray
      val variables = parameters.variables filter (_.typee == tpe) toArray
      val constants = parameters.constants(tpy)
    }
  }
  
  def copySymbols
}
