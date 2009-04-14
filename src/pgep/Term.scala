package pgep

abstract class Term {
  def nparams = 0
}

case class NextConst() extends Term

case class ConstKind() extends Term
case class Const(name: String, typee: Class[_], value: Any) extends ConstKind {
  def toExpressionString = name + "[" + value + "]"
}

case class VarKind() extends Term
case class Var(name: String, typee: Class[_]) extends VarKind {
  def toExpressionString = name
}

case class FuncKind() extends Term
case class Func(name: String, parameterTypes: List[Class[_]], resultType: Class[_],
                fn: Seq[Any] => Any, strFn: Seq[String] => String) extends FuncKind {
  def apply(values: Seq[Any], paramPos: Int) = fn(values slice (paramPos, values.length))
  def toExpressionString(values: Seq[String], paramPos: Int) = strFn(values slice (paramPos, values.length))
  
  override val nparams = parameterTypes.length
}

