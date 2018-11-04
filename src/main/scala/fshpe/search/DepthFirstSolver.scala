package fshpe.search

import fshpe.planning.SubProblem

case class DepthFirstSolver[S](problem: SubProblem[S], maxDepth: Long = Long.MaxValue) extends Solver[S] {

  override def subProblems: Stream[SubProblem[S]] = {
    val nextNodes = if (maxDepth > 0) problem.refinements.flatMap(DepthFirstSolver(_, maxDepth - 1).subProblems)
                    else Stream.empty
    problem #:: nextNodes
  }
}
