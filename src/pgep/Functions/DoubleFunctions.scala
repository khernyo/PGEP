package pgep.Functions

object DoubleFunctions {
  val add = Func(Symbol("+"), List(classOf[Double], classOf[Double]), classOf[Double],
                 (ns: Seq[Any]) => (ns(0), ns(1)) match {
                   case (n1: Double, n2: Double) => n1 + n2},
                 (ns: Seq[String]) => (ns(0), ns(1)) match {
                   case (s1, s2) => "("+ s1 +" + "+ s2 +")"})
  val add3 = Func(Symbol("+"), List(classOf[Double], classOf[Double], classOf[Double]), classOf[Double],
                 (ns: Seq[Any]) => (ns(0), ns(1), ns(2)) match {
                   case (n1: Double, n2: Double, n3: Double) => n1 + n2 + n3},
                 (ns: Seq[String]) => (ns(0), ns(1), ns(2)) match {
                   case (s1, s2, s3) => "("+ s1 +" + "+ s2 +" + "+ s3 +")"})
  val sub = Func(Symbol("+"), List(classOf[Double], classOf[Double]), classOf[Double],
                 (ns: Seq[Any]) => (ns(0), ns(1)) match {
                   case (n1: Double, n2: Double) => n1 - n2},
                 (ns: Seq[String]) => (ns(0), ns(1)) match {
                   case (s1, s2) => "("+ s1 +" - "+ s2 +")"})
  val mul = Func(Symbol("+"), List(classOf[Double], classOf[Double]), classOf[Double],
                 (ns: Seq[Any]) => (ns(0), ns(1)) match {
                   case (n1: Double, n2: Double) => n1 * n2},
                 (ns: Seq[String]) => (ns(0), ns(1)) match {
                   case (s1, s2) => "("+ s1 +" * "+ s2 +")"})
  val div = Func(Symbol("+"), List(classOf[Double], classOf[Double]), classOf[Double],
                 (ns: Seq[Any]) => (ns(0), ns(1)) match {
                   case (n1: Double, n2: Double) => n1 / n2},
                 (ns: Seq[String]) => (ns(0), ns(1)) match {
                   case (s1, s2) => "("+ s1 +" / "+ s2 +")"})

}
