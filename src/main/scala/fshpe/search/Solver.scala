package fshpe.search

import fshpe.planning.{Plan, SubProblem}

case class Solution[S](plan: Plan[S], cost: Double)

trait Solver[S] {

  def subProblems: Stream[SubProblem[S]]

  def solutions: Stream[Solution[S]] = subProblems.filter(_.taskNetwork.isEmpty).map(p => Solution(p.plan, p.cost))

  def firstSolution: Option[Solution[S]] = solutions.headOption
}
