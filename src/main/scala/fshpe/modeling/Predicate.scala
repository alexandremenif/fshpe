package fshpe.modeling

class Predicate[A](elements: Set[A]) {

  def apply(o: A): Boolean = elements.contains(o)

  def add(o: A): Predicate[A] = new Predicate(elements + o)

  def delete(o: A): Predicate[A] = new Predicate(elements - o)

  def toLazyList: LazyList[A] = elements.to(LazyList)

  override def toString: String = elements.toString
}

object Predicate {

  def apply[A](elements: A*): Predicate[A] = new Predicate(Set(elements:_*))
}
