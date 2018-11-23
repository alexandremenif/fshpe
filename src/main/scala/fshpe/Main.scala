package fshpe

import scala.concurrent.duration._

import fshpe.domains.Gripper.{AchieveAt, State}
import fshpe.modeling.{Object, Predicate}
import fshpe.planning.{Network, SubProblem}
import fshpe.search.{AnytimeDepthFirstSolver, DepthFirstSolver}
import fshpe.util.time

object Main extends App {


  val problem1: SubProblem[State] = {
    val gripper1 = Object("gripper1")
    val gripper2 = Object("gripper2")
    val ball1 = Object("ball1")
    val ball2 = Object("ball2")
    val room1 = Object("room1")
    val room2 = Object("room2")
    val room3 = Object("room3")

    SubProblem(
      State(
        room = Predicate(room1, room2, room3),
        ball = Predicate(ball1, ball2),
        gripper = Predicate(gripper1, gripper2),
        atRobby = Predicate(room1),
        at = Predicate((ball1, room1), (ball2, room2)),
        free = Predicate(gripper1, gripper2),
        carry = Predicate()
      ),
      Network.parallel(
        AchieveAt(ball1, room3),
        AchieveAt(ball2, room3)
      )
    )
  }

  val problem2: SubProblem[State] = {
    val gripper1 = Object("gripper1")
    val gripper2 = Object("gripper2")
    val ball1 = Object("ball1")
    val ball2 = Object("ball2")
    val ball3 = Object("ball3")
    val room1 = Object("room1")
    val room2 = Object("room2")
    val room3 = Object("room3")
    val room4 = Object("room4")

    SubProblem(
      State(
        room = Predicate(room1, room2, room3, room4),
        ball = Predicate(ball1, ball2, ball3),
        gripper = Predicate(gripper1, gripper2),
        atRobby = Predicate(room1),
        at = Predicate((ball1, room1), (ball2, room2), (ball3, room3)),
        free = Predicate(gripper1, gripper2),
        carry = Predicate()
      ),
      Network.parallel(
        AchieveAt(ball1, room2),
        AchieveAt(ball2, room3),
        AchieveAt(ball3, room4)
      )
    )
  }

  val problem3: SubProblem[State] = {
    val gripper1 = Object("gripper1")
    val gripper2 = Object("gripper2")
    val gripper3 = Object("gripper3")
    val ball1 = Object("ball1")
    val ball2 = Object("ball2")
    val ball3 = Object("ball3")
    val ball4 = Object("ball4")
    val ball5 = Object("ball5")
    val ball6 = Object("ball6")
    val ball7 = Object("ball7")
    val ball8 = Object("ball8")
    val ball9 = Object("ball9")
    val ball10 = Object("ball10")
    val room1 = Object("room1")
    val room2 = Object("room2")
    val room3 = Object("room3")
    val room4 = Object("room4")
    val room5 = Object("room5")

    SubProblem(
      State(
        room = Predicate(room1, room2, room3, room4, room5),
        ball = Predicate(ball1, ball2, ball3, ball4, ball5, ball6, ball7, ball8, ball9, ball10),
        gripper = Predicate(gripper1, gripper2, gripper3),
        atRobby = Predicate(room1),
        at = Predicate(
          (ball1, room2),
          (ball2, room2),
          (ball3, room3),
          (ball5, room3),
          (ball6, room5),
          (ball7, room4),
          (ball8, room1),
          (ball9, room4),
          (ball10, room1)
        ),
        free = Predicate(gripper1, gripper2),
        carry = Predicate((ball4, gripper3))
      ),
      Network.parallel(
        AchieveAt(ball1, room3),
        AchieveAt(ball2, room3),
        AchieveAt(ball3, room5),
        AchieveAt(ball4, room5),
        AchieveAt(ball5, room4),
        AchieveAt(ball6, room1),
        AchieveAt(ball7, room5),
        AchieveAt(ball8, room3),
        AchieveAt(ball9, room1),
        AchieveAt(ball10, room5)
      )
    )
  }

  val problem4: SubProblem[State] = {
    val gripper1 = Object("gripper1")
    val gripper2 = Object("gripper2")
    val ball1 = Object("ball1")
    val ball2 = Object("ball2")
    val room1 = Object("room1")
    val room2 = Object("room2")
    val room3 = Object("room3")

    SubProblem(
      State(
        room = Predicate(room1, room2, room3),
        ball = Predicate(ball1, ball2),
        gripper = Predicate(gripper1, gripper2),
        atRobby = Predicate(room1),
        at = Predicate(
          (ball1, room2),
          (ball2, room2)
        ),
        free = Predicate(gripper1, gripper2),
        carry = Predicate()
      ),
      Network.parallel(
        AchieveAt(ball1, room3),
        AchieveAt(ball2, room3)
      )
    )
  }

  val solutions = time {
    AnytimeDepthFirstSolver(problem3).solutions.dropWhile(_.cost > 37).take(1).force
  }

  solutions.foreach(println)
}
