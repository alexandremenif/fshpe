package fshpe.planning

sealed trait Task[S]

abstract class Action[S] extends Task[S] {

  def precondition(state: S): Boolean = true

  def effects(state: S): S = state

  def cost(state: S): Double = 1d

  final def apply(state: S): Option[S] = if(precondition(state)) Some(effects(state)) else None
}

abstract class CompoundTask[S] extends Task[S] {

  def methods: Stream[Method[S]]

  final def decompositions(state: S): Stream[TaskNetwork[S]] = methods.flatMap(_.decompositions(state))
}