package pgep

import scala.collection.Map
import scala.collection.mutable.{HashMap,HashSet}

object Gene {
  def apply(parameters: GeneParameters) = {
    val k_expr = new HashMap[Class[_], Array[Term]]
    val consts = new HashMap[Class[_], Array[Const]]
    
    val types = new HashSet[Class[_]]
    types ++= parameters.functions map (_.resultType)
    types ++= parameters.variables map (_.typee)
    types ++= parameters.constants.keys
    
    types foreach { t =>
      k_expr(t) = new Array(parameters.headLen + parameters.tailLen)
      consts(t) = new Array(parameters.tailLen)
    }
    
    new Gene(parameters, k_expr, consts)
  }
  
  def copySymbols(src: Gene, dst: Gene, srcStart: Int, dstStart: Int, length: Int) {
    val tmp = new Array[Term](length)
    src._k_expression.keys foreach {tpe => 
      copySymbols(tpe, src, tmp, srcStart, 0, length)
      copySymbols(tpe, tmp, dst, 0, dstStart, length)
    }
    assert(dst.consistent)
  }
  
  protected def copySymbols(tpe: Class[_], src: Gene, dst: Array[Term], srcStart: Int, dstStart: Int, length: Int) {
    val k_expr = src._k_expression(tpe)
    val consts = src._constants(tpe)
    if (srcStart < k_expr.length) {
      val len = Math.min(srcStart + length, k_expr.length) - srcStart
      Array.copy(k_expr, srcStart, dst, dstStart, len)
    }
    if (srcStart + length > k_expr.length) {
      val start = Math.max(srcStart - k_expr.length, 0)
      Array.copy(consts, start, dst, dstStart + Math.max(k_expr.length - srcStart, 0),
                 srcStart + length - k_expr.length - start);
    }
  }

  protected def copySymbols(tpe: Class[_], src: Array[Term], dst: Gene, srcStart: Int, dstStart: Int, length: Int) {
    val k_expr = dst._k_expression(tpe)
    val consts = dst._constants(tpe)
    if (dstStart < k_expr.length) {
      val len = Math.min(dstStart + length, k_expr.length) - dstStart
      Array.copy(src, srcStart, k_expr, dstStart, len)
    }
    if (dstStart + length > k_expr.length) {
      val start = Math.max(dstStart - k_expr.length, 0)
      Array.copy(src, srcStart + Math.max(k_expr.length - dstStart, 0), consts, start,
                 dstStart + length - k_expr.length - start)
    }
  }
}

class Gene(parameters: GeneParameters, k_expression: Map[Class[_], Array[Term]], constants: Map[Class[_], Array[Const]]) {
  val _k_expression = k_expression
  private val _constants = constants
  
  private val random = RNGProvider()
  
  def randomize(selector_fvc: Selector[Term], selector_vc: Selector[Term],
  				headp: () => Boolean, tailp: () => Boolean, constp: () => Boolean) {
    for (tpe <- _k_expression.keys) {
      val k_expr = _k_expression(tpe)
      val functions = parameters.functions filter (_.resultType == tpe) toArray
      val variables = parameters.variables filter (_.typee == tpe) toArray
      val constants = parameters.constants(tpe)
      
      for (i <- 0 until parameters.headLen)
        if (headp())
          k_expr(i) = selector_fvc() match {
            case FuncKind() => functions(random.nextInt(functions.length))
            case VarKind() => variables(random.nextInt(variables.length))
            case ConstKind() => NextConst()
          }
      
      for (i <- parameters.headLen until parameters.headLen + parameters.tailLen)
        if (tailp())
          k_expr(i) = selector_vc() match {
            case VarKind() => variables(random.nextInt(variables.length))
            case ConstKind() => NextConst()
          }
      
      for (i <- 0 until parameters.tailLen)
        if (constp())
          _constants(tpe)(i) = constants(random.nextInt(constants.length))
    }
    
    assert(consistent)
  }
  
  protected def evalWalk(resultTypes: Array[Class[_]]) = new Iterator[Tuple3[Term, Int, Any]] {
    private val (lastParameterPos, lastIdx) = {
      var sum = 0
      val lastParameterPos = (0 until parameters.headLen + parameters.tailLen) map (i => {sum += _k_expression(resultTypes(i))(i).nparams; sum}) toArray
      val lastIdx = lastParameterPos.zipWithIndex map {case (pos, i) => (pos == i)} indexOf (true)
      (lastParameterPos, lastIdx)
    }

    private var i = lastIdx
    private var constIdx = 0
    
    def hasNext = i >= 0 
    def next() = {
      val tpe = resultTypes(i)
      val term = _k_expression(tpe)(i)
      val result = term match {
        case NextConst() => {constIdx += 1; (_constants(tpe)(constIdx - 1), i, null)}
        case v: Var => (v, i, null)
        case f: Func => (f, i, lastParameterPos(i) - term.nparams + 1)
      }
      i -= 1
      result
    }
  }
  
  protected def buildResultTypes: Array[Class[_]] = {
    val resultTypes = new Array[Class[_]](parameters.headLen + parameters.tailLen)
    resultTypes(0) = parameters.resultType

    var i = 0
    var paramPos = 0
    while (i <= paramPos) {
      _k_expression(resultTypes(i))(i) match {
        case f: Func => f.parameterTypes foreach ( tpe => {paramPos += 1; resultTypes(paramPos) = tpe})
        case _ =>
      }
      i += 1
    }
    assert(expressionTypeConsistent(resultTypes))
    resultTypes
  }
  
  protected def consistent = {
    _k_expression.forall {
      case (tpe, terms) => terms forall {
        case null => true
        case NextConst() => true
        case v: Var => v.typee == tpe
        case f: Func => f.resultType == tpe
        case _ => false
      }
    } && _constants.forall {
      case (tpe, const) => const forall {
        case null => true
        case c: Const => c.typee == tpe
      }
    }
  }
  
  protected def expressionTypeConsistent(resultTypes: Array[Class[_]]) =
    resultTypes(0) == parameters.resultType &&
      (evalWalk(resultTypes) forall {
        case (f: Func, idx, paramPos: Int) => {(0 until f.nparams) forall (i => {f.parameterTypes(i) == resultTypes(paramPos + i)})}
        case e @ (f: Func, idx, _) => error("Aieeeee: " + e)
        case _ => true
      })
  
  def toExpressionString = {
    assert(consistent)
    val resultTypes = buildResultTypes
    
    val values = new Array[String](parameters.headLen + parameters.tailLen)
    evalWalk(resultTypes) foreach {
      case (c: Const, idx, _) => values(idx) = c.toExpressionString
      case (v: Var, idx, _) => values(idx) = v.toExpressionString
      case (f: Func, idx, paramPos: Int) => values(idx) = f.toExpressionString(values, paramPos)
      case e => error("Aieeeeeeeee: " + e)
    }
    values(0)
  }
  
  def apply(variables: Map[String, Any]) = {
    assert(consistent)
    val resultTypes = buildResultTypes
    
    val values = new Array[Any](parameters.headLen + parameters.tailLen)
    evalWalk(resultTypes) foreach {
      case (c: Const, idx, _) => values(idx) = c.value
      case (v: Var, idx, _) => values(idx) = variables(v.name)
      case (f: Func, idx, paramPos: Int) => values(idx) = f(values, paramPos)
      case e => error("Aieeeeeeeee: " + e)
    }
    values(0)
  }
  
  def cloneConsts() {
    for (tpe <- _constants.keys)
      for (i <- (0 until parameters.tailLen))
        _constants(tpe)(i) = _constants(tpe)(i).clone()
  }
  
  override def clone() = {
    val cloned = Gene(parameters)
    for (tpe <- _k_expression.keys)
      Array.copy(_k_expression(tpe), 0, cloned._k_expression(tpe), 0, _k_expression(tpe).length)
    for (tpe <- _constants.keys)
      Array.copy(_constants(tpe), 0, cloned._constants(tpe), 0, _constants(tpe).length)
    
    cloned
  }
}
