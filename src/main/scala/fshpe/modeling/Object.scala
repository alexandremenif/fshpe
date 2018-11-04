package fshpe.modeling

class Object(private val symbol: String) {

  override def toString: String = symbol
}

object Object {

  def apply(symbol: String): Object = new Object(symbol)
}
