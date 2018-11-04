package fshpe.domains

import fshpe.modeling.{Object, Predicate}
import fshpe.planning._

object Gripper {

  case class State(
    room: Predicate[Object],
    ball: Predicate[Object],
    gripper: Predicate[Object],
    atRobby: Predicate[Object],
    at: Predicate[(Object, Object)],
    free: Predicate[Object],
    carry: Predicate[(Object, Object)]
  )

  case class Move(from: Object, to: Object) extends Action[State] {

    override def precondition(state: State): Boolean = state.room(from) && state.room(to) && state.atRobby(from)

    override def effects(state: State): State = state.copy(
      atRobby = state.atRobby
        .delete(from)
        .add(to)
    )
  }

  case class Pick(obj: Object, room: Object, gripper: Object) extends Action[State] {

    override def precondition(state: State): Boolean =
      state.ball(obj) && state.room(room) && state.gripper(gripper) &&
        state.at(obj, room) && state.atRobby(room) && state.free(gripper)

    override def effects(state: State): State = state.copy(
      at = state.at.delete(obj, room),
      carry = state.carry.add(obj, gripper),
      free = state.free.delete(gripper)
    )
  }

  case class Drop(obj: Object, room: Object, gripper: Object) extends Action[State] {

    override def precondition(state: State): Boolean =
      state.ball(obj) && state.room(room) && state.gripper(gripper) && state.carry(obj, gripper) && state.atRobby(room)

    override def effects(state: State): State = state.copy(
      carry = state.carry.delete(obj, gripper),
      at = state.at.add(obj, room),
      free = state.free.add(gripper)
    )
  }

  case class AchieveAtRobby(room: Object) extends CompoundTask[State] {

    override def methods: Stream[Method[State]] = Stream(
      Method(_.atRobby(room), Network.empty),
      Method { state =>
        for {
          from <- state.atRobby.toStream  if from != room
        } yield Network.one[Task[State]](Move(from, room))
      }
    )
  }

  case class AchieveFree(gripper: Object) extends CompoundTask[State] {

    override def methods: Stream[Method[State]] = Stream(
      Method(_.free(gripper), Network.empty),
      Method { state =>
        for {
          obj <- state.ball.toStream if state.carry(obj, gripper)
          room <- state.room.toStream
        } yield Network.sequence[Task[State]](
          AchieveAtRobby(room),
          Drop(obj, room, gripper)
        )
      }
    )
  }

  case class AchieveCarry(gripper: Object, obj: Object) extends CompoundTask[State] {

    override def methods: Stream[Method[State]] = Stream(
      Method(_.carry(obj, gripper), Network.empty),
      Method { state =>
        for {
          room <- state.room.toStream if state.at(obj, room)
        } yield Network.sequence[Task[State]](
          AchieveFree(gripper),
          AchieveAtRobby(room),
          Pick(obj, room, gripper)
        )
      }
    )
  }

  case class AchieveAt(obj: Object, room: Object) extends CompoundTask[State] {

    override def methods: Stream[Method[State]] = Stream(
      Method(_.at(obj, room), Network.empty),
      Method { state =>
        for {
          gripper <- state.gripper.toStream
        } yield Network.sequence[Task[State]](
          AchieveCarry(gripper, obj),
          AchieveAtRobby(room),
          Drop(obj, room, gripper)
        )
      }
    )
  }
}
