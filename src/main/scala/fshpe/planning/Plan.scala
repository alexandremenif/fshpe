package fshpe.planning

class Plan[S] private (private val actions: List[Action[S]]) extends Seq[Action[S]] {

  val iterator: Iterator[Action[S]] = actions.reverseIterator

  override def apply(idx: Int): Action[S] = actions(actions.size - idx - 1)

  override def length: Int = actions.length

  def append(action: Action[S]): Plan[S] = new Plan(action :: actions)
}

object Plan {

  def empty[S]: Plan[S] = new Plan(Nil)
}