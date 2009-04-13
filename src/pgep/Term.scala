package pgep

sealed abstract class Term {
  val nparams = 0
}

case class Const(name: String, typee: Class[_], value: Any) extends Term
case class Var(name: String, typee: Class[_]) extends Term
case class Func(name: String, parameterTypes: List[Class[_]], resultType: Class[_], fn: List[Any] => Any, strFn: List[String] => String) extends Term {
  def apply = fn
  def applyStr = strFn
  
  override val nparams = parameterTypes.length
}