package fshpe.search

import fshpe.planning.SubProblem

case class AnytimeDepthFirstSolver[S](
  problem: SubProblem[S],
  maxDepth: Long = Long.MaxValue,
  maxCost: Double = Double.PositiveInfinity
) extends Solver[S] {

  override def subProblems: LazyList[SubProblem[S]] = subProblems((problem, maxDepth) #:: LazyList.empty, maxCost).map(_._1)

  private def subProblems(stack: LazyList[(SubProblem[S], Long)], bound: Double): LazyList[(SubProblem[S], Long)] = {
    stack.headOption
      .map { case (subProblem, depth) =>
        if (depth > 0 && subProblem.cost < bound) {
          val updatedBound = if (subProblem.taskNetwork.isEmpty) subProblem.cost else bound
          (subProblem, depth) #:: subProblems(subProblem.refinements.map(sp => (sp, depth - 1)) #::: stack.tail, updatedBound)
        } else {
          subProblems(stack.tail, bound)
        }
      }
      .getOrElse(LazyList.empty)
  }
}
