package fshpe.planning

trait Method[S] {

  def decompositions(state: S): Stream[TaskNetwork[S]]
}

object Method {

  def apply[S](decomposition: TaskNetwork[S]): Method[S] = new Method[S] {
    override def decompositions(state: S): Stream[TaskNetwork[S]] = decomposition #:: Stream.empty[TaskNetwork[S]]
  }

  def apply[S](precondition: S => Boolean, decomposition: => TaskNetwork[S]): Method[S] = new Method[S] {
    override def decompositions(state: S): Stream[TaskNetwork[S]] =
      if (precondition(state)) decomposition #:: Stream.empty[TaskNetwork[S]] else Stream.empty[TaskNetwork[S]]
  }

  def apply[S](decompositionsF: S => Stream[TaskNetwork[S]]): Method[S] = new Method[S] {
    override def decompositions(state: S): Stream[TaskNetwork[S]] = decompositionsF(state)
  }
}