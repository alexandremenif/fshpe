package fshpe.planning

import org.scalatest.{FunSuite, Matchers}

class NetworkTest extends FunSuite with Matchers {

  test("Nodes equality") {
    val n1 = Node(1)
    val n2 = Node(2)
    val n3 = Node(1)

    n1 == n1 should be (true)
    n1 == n2 should be (false)
    n1 == n3 should be (false)
   }

  test("Empty network") {
    val network = Network.empty

    network.isEmpty should be (true)
    network.nodes shouldBe empty
    network.firstNodes shouldBe empty
  }

  test("Singleton network") {
    val network = Network.one(1)

    network.isEmpty should be (false)
    network.nodes should contain theSameElementsAs Seq(1)
    network.firstNodes should contain theSameElementsAs Seq(1)
  }

  test("Sequence network") {
    val network = Network.sequence(1, 2, 3)

    network.isEmpty should be (false)
    network.nodes.map(_.element) should contain theSameElementsInOrderAs Seq(1, 2, 3)
    network.firstNodes.map(_.element) should contain theSameElementsAs Seq(1)
  }

  test("Parallel network") {
    val network = Network.parallel(1, 2, 3)

    network.isEmpty should be (false)
    network.nodes.map(_.element) should contain theSameElementsAs Seq(1, 2, 3)
    network.firstNodes.map(_.element) should contain theSameElementsAs Seq(1, 2, 3)
  }

  test("S shaped network") {
    val n4 = Node(4)
    val n3 = Node(3)
    val n2 = Node(2, Set(n4))
    val n1 = Node(1, Set(n3, n4))
    val network = n1 +:: n2 +:: n3 +:: n4 +:: Network.empty

    network.isEmpty should be (false)
    network.nodes.map(_.element) should contain theSameElementsInOrderAs Seq(1, 2, 3, 4)
    network.firstNodes.map(_.element) should contain theSameElementsAs Seq(1, 2)
  }

  test("Pop while on sequence network") {
    val (heads, network) = Network.sequence(1, 2, 3, 4, 5).popWhile(node => node.element < 4)

    network.isEmpty should be (false)
    network.nodes.map(_.element) should contain theSameElementsInOrderAs Seq(4, 5)
    network.firstNodes.map(_.element) should contain theSameElementsAs Seq(4)
    heads.map(_.element) should contain theSameElementsInOrderAs Seq(3, 2, 1)
  }

  test("Pop while on parallel network") {
    val (heads, network) = Network.parallel(1, 2, 3, 4, 5).popWhile(node => node.element < 4)

    network.isEmpty should be (false)
    network.nodes.map(_.element) should contain theSameElementsInOrderAs Seq(4, 5)
    network.firstNodes.map(_.element) should contain theSameElementsAs Seq(4, 5)
    heads.map(_.element) should contain theSameElementsInOrderAs Seq(3, 2, 1)
  }


  test("Node removal") {
    val n3 = Node(3)
    val n2 = Node(2, Set(n3))
    val n1 = Node(1, Set(n3))
    val network1 = (n1 +:: n2 +:: n3 +:: Network.empty).removed(n3)

    network1.isEmpty should be (false)
    network1.nodes.map(_.element) should contain theSameElementsInOrderAs Seq(1, 2)
    network1.firstNodes.map(_.element) should contain theSameElementsAs Seq(1, 2)

    val network2 = (n1 +:: n2 +:: n3 +:: Network.empty).removed(n1)

    network2.isEmpty should be (false)
    network2.nodes.map(_.element) should contain theSameElementsInOrderAs Seq(2, 3)
    network2.firstNodes.map(_.element) should contain theSameElementsAs Seq(2)
  }

  test("Replacement in sequence network") {
    val network = Network.sequence(1, 2)
    val subNetwork = Network.parallel(11, 12)
    val node = network.nodes(0)

    val network1 = network.replaced(node, subNetwork)

    network1.isEmpty should be (false)
    network1.nodes.map(_.element) should contain theSameElementsInOrderAs Seq(11, 12, 2)
    network1.firstNodes.map(_.element) should contain theSameElementsAs Seq(11, 12)
  }

  test("Replacement in parallel network") {
    val network = Network.parallel(1, 2, 3)
    val subNetwork = Network.sequence(21, 22)
    val node = network.nodes(1)

    val network1 = network.replaced(node, subNetwork)

    network1.isEmpty should be (false)
    network1.nodes.map(_.element) should contain theSameElementsInOrderAs Seq(1, 21, 22, 3)
    network1.firstNodes.map(_.element) should contain theSameElementsAs Seq(1, 21, 3)
  }
}
