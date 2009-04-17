package pgep

abstract class Alphabet[T <: Term](ts: T*) extends Seq[T] {
  protected val terms = ts.toArray

  protected def _maxParams = terms map (_.nparams) reduceLeft Math.max 
  def maxParams: Int
  override def length = terms.length
  override def elements = terms.elements
  override def apply(i: Int): T = terms.apply(i)
}

class AlphabetRO[T <: Term](ts: T*) extends Alphabet[T](ts: _*) {
  val maxParams = _maxParams
}

class AlphabetRW[T <: Term](ts: T*) extends Alphabet[T](ts: _*) {
  var maxParams = _maxParams
  def update(i: Int, v: T) {terms(i) = v; maxParams = _maxParams}
}
