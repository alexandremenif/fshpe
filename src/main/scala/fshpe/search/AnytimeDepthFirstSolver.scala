package fshpe.search

import fshpe.planning.SubProblem

import scala.annotation.tailrec

case class AnytimeDepthFirstSolver[S](problem: SubProblem[S], maxDepth: Long = Long.MaxValue, maxCost: Double = Double.PositiveInfinity) extends Solver[S] {

  override def subProblems: Stream[SubProblem[S]] = subProblems(problem #:: Stream.empty[SubProblem[S]], maxDepth, maxCost)

  private def subProblems(stack: Stream[SubProblem[S]], depth: Long, bound: Double): Stream[SubProblem[S]] = {

    stack match {
      case Stream() => Stream.empty
      case problem #:: tail =>
        if (depth > 0 && problem.cost < bound) {
          problem #:: subProblems(
            problem.refinements #::: tail,
            depth,
            if (problem.taskNetwork.isEmpty) problem.cost else bound
          )
        } else {
          subProblems(tail, depth, bound)
        }
    }
  }
}
