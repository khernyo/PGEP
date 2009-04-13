package pgep

import scala.collection.mutable.{HashMap,HashSet}

object Gene {
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
/*  
  protected def evalWalk(cfn: (Int, Const) => Unit, vfn: (Int, Var) => Unit, ffn: (Int, Func, Int) => Unit, resultTypes: Array[Class[_]]) {
    // ezt az egeszet at kellene irni iteratorra
    var constIdx = 0
    val k_expr = new Array[Term](resultTypes.length)
    
    val lastParameterPos = new Array[Int](parameters.headLen + parameters.tailLen)
    var last = 0
    var sum = 0
    var i = 0
    while (i < parameters.headLen + parameters.tailLen && sum != (i - 1)) {
      val term = _k_expression(resultTypes(i))(i)
      k_expr(i) = term
      
      term match {
        case NextConst => {cfn(i, _constants(resultTypes(i))(constIdx)); constIdx += 1}
        case Var(_, _) => vfn(i, term.asInstanceOf[Var])
        case Func => sum += term.nparams
      }
      
      lastParameterPos(i) = sum
      
      last = i
      i += 1
    }
    
    for (i <- new Range(last, -1, -1))
      k_expr(i) match {
        case Func => ffn(i, k_expr(i).asInstanceOf[Func], lastParameterPos(i) - k_expr(i).nparams + 1)
        case _ =>
      }
  }
*/
  protected def evalWalk(cfn: (Int, Const) => Unit, vfn: (Int, Var) => Unit, ffn: (Int, Func, Int) => Unit, resultTypes: Array[Class[_]]) {
    // ezt az egeszet at kellene irni iteratorra
    var sum = 0
    val lastParameterPos = (0 until parameters.headLen + parameters.tailLen) map (i => {sum += _k_expression(resultTypes(i))(i).nparams; sum}) toArray
    val lastIdx = lastParameterPos.zipWithIndex map {case (pos, i) => (pos == i)} indexOf (true)
    
    var constIdx = 0
    for (i <- new Range(lastIdx, -1, -1)) {
      val tpe = resultTypes(i)
      val term = _k_expression(tpe)(i)
      term match {
        case NextConst() => {cfn(i, _constants(tpe)(constIdx)); constIdx += 1}
        case v: Var => vfn(i, v)
        case f: Func => ffn(i, f, lastParameterPos(i) - term.nparams + 1)
      }
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
  
  protected def expressionTypeConsistent(resultTypes: Array[Class[_]]) = {
    var consistent = resultTypes(0) == parameters.resultType
    evalWalk((idx, c) => { },
    		 (idx, v) => { },
    		 (idx, f, paramPos) => {for (i <- 0 until f.nparams) {consistent &= f.parameterTypes(i) == resultTypes(paramPos + i)}},
    		 resultTypes)
    consistent      
  }
  
  def toExpressionString = {
    assert(consistent)
    val resultTypes = buildResultTypes
    
    val values = new Array[String](parameters.headLen + parameters.tailLen)
    evalWalk((idx, c) => values(idx) = c.toExpressionString,
             (idx, v) => values(idx) = v.toExpressionString,
             (idx, f, paramPos) => values(idx) = f.toExpressionString(values, paramPos),
             resultTypes)
    values(0)
  }
  
  def apply(variables: Map[String, Any]) {
    assert(consistent)
    val resultTypes = buildResultTypes
    
    val values = new Array[Any](parameters.headLen + parameters.tailLen)
    evalWalk((idx, c) => values(idx) = c.value,
             (idx, v) => values(idx) = variables(v.name),
             (idx, f, paramPos) => values(idx) = f(values, paramPos),
             resultTypes)
    values(0)
  }
}
