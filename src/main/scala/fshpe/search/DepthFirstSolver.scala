package fshpe.search

import fshpe.planning.SubProblem

case class DepthFirstSolver[S](problem: SubProblem[S], maxDepth: Long = Long.MaxValue) extends Solver[S] {

  override def subProblems: LazyList[SubProblem[S]] = subProblems(problem, maxDepth)

  private def subProblems(subProblem: SubProblem[S], maxDepth: Long): LazyList[SubProblem[S]] = {
    if (maxDepth > 0)
      subProblem #:: subProblem.refinements.flatMap(subProblems(_, maxDepth - 1))
    else
      LazyList.empty
  }
}
