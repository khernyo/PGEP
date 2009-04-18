package pgep

abstract class Term {
  def nparams = 0
}

case class NextConst() extends Term

case class ConstKind() extends Term
case class Const(name: Symbol, typ: Class[_], value: Any) extends ConstKind {
  def toExpressionString(): String = name + "[" + value + "]"
  override def clone() = Const(name, typ, value)
}

case class VarKind() extends Term
case class Var(name: Symbol, typ: Class[_]) extends VarKind {
  def toExpressionString(): String = name.name
}

case class FuncKind() extends Term
case class Func(name: Symbol, parameterTypes: List[Class[_]], resultType: Class[_],
                fn: Seq[Any] => Any, strFn: Seq[String] => String) extends FuncKind {
  def apply(values: Seq[Any], paramPos: Int) = fn(values.drop(paramPos).take(parameterTypes.length))
  def toExpressionString(values: Seq[String], paramPos: Int): String = strFn(values.drop(paramPos).take(parameterTypes.length))
  
  override val nparams = parameterTypes.length
}

