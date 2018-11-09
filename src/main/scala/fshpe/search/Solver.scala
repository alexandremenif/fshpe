package fshpe.search

import fshpe.planning.{Plan, SubProblem}

import scala.concurrent.duration.Duration

case class Solution[S](plan: Plan[S], cost: Double)

trait Solver[S] {

  def subProblems: LazyList[SubProblem[S]]

  def solutions: LazyList[Solution[S]] = subProblems.filter(_.taskNetwork.isEmpty).map(p => Solution(p.plan, p.cost))

  def firstSolution: Option[Solution[S]] = solutions.headOption

  /**
    * @param maxDuration max duration in milliseconds
    * @return best solution found within the max duration
    */
  def solutionsWithin(maxDuration: Duration): LazyList[Solution[S]] = {
    val start = System.nanoTime()
    subProblems
      .takeWhile(_ => (System.nanoTime() - start) < maxDuration.toNanos)
      .filter(_.taskNetwork.isEmpty)
      .map(p => Solution(p.plan, p.cost))
  }
}
