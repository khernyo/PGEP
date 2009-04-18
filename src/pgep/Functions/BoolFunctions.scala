package pgep.Functions

object BoolFunctions {
  val and = Func(Symbol("&"), List(classOf[Boolean], classOf[Boolean]), classOf[Boolean],
                 (ns: Seq[Any]) => (ns(0), ns(1)) match {
                   case (n1: Boolean, n2: Boolean) => n1 & n2},
                 (ns: Seq[String]) => (ns(0), ns(1)) match {
                   case (s1, s2) => "("+ s1 +" & "+ s2 +")"})
  val or = Func(Symbol("|"), List(classOf[Boolean], classOf[Boolean]), classOf[Boolean],
                 (ns: Seq[Any]) => (ns(0), ns(1)) match {
                   case (n1: Boolean, n2: Boolean) => n1 | n2},
                 (ns: Seq[String]) => (ns(0), ns(1)) match {
                   case (s1, s2) => "("+ s1 +" | "+ s2 +")"})
  val not = Func(Symbol("Not"), List(classOf[Boolean]), classOf[Boolean],
                 (ns: Seq[Any]) => ns(0) match {
                   case n: Boolean => !n},
                 (ns: Seq[String]) => ns(0) match {
                   case s => "Not("+ s +")"})
}
