package fshpe.planning

import scala.annotation.tailrec

case class Node[A](element: A, nextNodes: Set[Node[A]] = Set.empty[Node[A]]) {

  def withNextNodes(nodes: Set[Node[A]]): Node[A] = copy(nextNodes = nodes)

  override def equals(obj: Any): Boolean = obj match {
    case node: Node[A] => eq(node)
    case _ => false
  }
}

sealed abstract class Network[A] {

  def +::(node: Node[A]): Network[A] = new +::(node, this)

  def firstNodes: LazyList[Node[A]]

  def isEmpty: Boolean

  def nodes: List[Node[A]]

  def pop: (Option[Node[A]], Network[A])

  def addAll(nodes: List[Node[A]]): Network[A] =
    nodes.foldLeft[Network[A]](this) { case (network, node) => node +:: network }

  def popWhile(p: Node[A] => Boolean): (List[Node[A]], Network[A]) = {

    @tailrec
    def popWhile(heads: List[Node[A]], network: Network[A]): (List[Node[A]], Network[A]) = network match {
      case Empty() => (heads, Empty())
      case head +:: tail if p(head) => popWhile(head :: heads, tail)
      case _ => (heads, network)
    }

    popWhile(Nil, this)
  }

  def popAll: List[Node[A]] = popWhile(_ => true)._1

  def removed(node: Node[A]): Network[A] = {

    val (heads, network) = popWhile(_ != node)
    val (_, networkWithoutNode) = network.pop

    networkWithoutNode.addAll(heads)
  }

  def replaced(node: Node[A], subNetwork: Network[A]): Network[A] = {
    val (heads, network) = popWhile(_ != node)
    val (_, networkWithoutNode) = network.pop
    val subNodes = subNetwork.popAll

    val (networkWithSubNodes, _) = subNodes.foldLeft[(Network[A], Map[Node[A], Node[A]])](networkWithoutNode, Map()) {
      case ((networkAcc, mappingAcc), n) =>
        val newNode = if (n.nextNodes.isEmpty) n.withNextNodes(node.nextNodes)
                      else n.withNextNodes(n.nextNodes.map(mappingAcc.withDefault(identity)))
        (newNode +:: networkAcc, mappingAcc.updated(n, newNode))
    }

    networkWithSubNodes.addAll(heads)
  }

  override def toString: String = {
    val nodeIndex = nodes.view.zipWithIndex.map { case (node, i) => (node, s"n$i")}.to(Map)
    val constraints = nodes.foldLeft[Set[(String, String)]](Set()) { case (acc, node) =>
      acc.union(node.nextNodes.map(n => (nodeIndex.getOrElse(node, "unknown"), nodeIndex.getOrElse(n, "unknown"))))
    }
    s"(${nodes.map(n => s"${nodeIndex(n)}: ${n.element}").mkString("{", ", ", "}")}, ${constraints.mkString("{", ", ", "}")})"
  }
}

case class +::[A](head: Node[A], tail: Network[A]) extends Network[A] {

  override def firstNodes: LazyList[Node[A]] = head #:: tail.firstNodes.filter(node => !head.nextNodes.contains(node))

  override def isEmpty: Boolean = false

  override def nodes: List[Node[A]] = head :: tail.nodes

  override def pop: (Option[Node[A]], Network[A]) = (Some(head), tail)
}

case class Empty[A]() extends Network[A] {

  override def firstNodes: LazyList[Node[A]] = LazyList.empty

  override def isEmpty: Boolean = true

  override def nodes: List[Node[A]] = Nil

  override def pop: (Option[Node[A]], Network[A]) = (None, this)
}

object Network {

  def empty[A]: Network[A] = Empty()

  def apply[A](nodes: Node[A]*): Network[A] = Empty().addAll(List(nodes:_*))

  def one[A](element: A): Network[A] = Node(element) +:: Empty()

  def sequence[A](elements: A*): Network[A] = elements.reverse.foldLeft[Network[A]](Empty()) {
    case (Empty(), element) => Node(element) +:: Empty()
    case (head +:: tail, element) => Node(element, Set(head)) +:: head +:: tail
  }

  def parallel[A](elements: A*): Network[A] = elements.reverse.foldLeft[Network[A]](Empty()) {
    case (network, element) => Node(element) +:: network
  }
}
