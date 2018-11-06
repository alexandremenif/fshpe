package fshpe.search

import fshpe.planning.SubProblem

import scala.annotation.tailrec

case class AnytimeDepthFirstSolver[S](
  problem: SubProblem[S],
  maxDepth: Long = Long.MaxValue,
  maxCost: Double = Double.PositiveInfinity
) extends Solver[S] {

  override def subProblems: Stream[SubProblem[S]] = subProblems((problem, maxDepth) #:: Stream.empty, maxCost).map(_._1)

  private def subProblems(stack: Stream[(SubProblem[S], Long)], bound: Double): Stream[(SubProblem[S], Long)] = {
    stack match {
      case (subProblem, depth) #:: tail =>
        if (depth > 0 && subProblem.cost < bound) {
          val updatedBound = if (subProblem.taskNetwork.isEmpty) subProblem.cost else bound
          (subProblem, depth) #:: subProblems(subProblem.refinements.map(sp => (sp, depth - 1)) #::: tail, updatedBound)
        } else {
          subProblems(tail, bound)
        }
      case _ =>
        Stream.empty
    }
  }
}
