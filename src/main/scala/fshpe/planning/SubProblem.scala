package fshpe.planning

case class SubProblem[S](state: S, taskNetwork: TaskNetwork[S], plan: Plan[S] = Plan.empty[S], cost: Double = 0d) {

  def refinements: LazyList[SubProblem[S]] = for {
    firstNode <- taskNetwork.firstNodes
    subProblem <- refinements(firstNode)
  } yield subProblem

  private def refinements(node: Node[Task[S]]): LazyList[SubProblem[S]] = node.element match {
    case action: Action[S] => action.apply(state)
      .map(s => SubProblem(s, taskNetwork.removed(node), plan.append(action), cost + action.cost(state)))
      .to(LazyList)
    case compoundTask: CompoundTask[S] => compoundTask.decompositions(state)
      .map(decomposition => SubProblem(state, taskNetwork.replaced(node, decomposition), plan, cost))
  }

  override def toString: String =
    s"""(
       |  state: $state,
       |  plan: $plan,
       |  network: $taskNetwork
       |)""".stripMargin
}
