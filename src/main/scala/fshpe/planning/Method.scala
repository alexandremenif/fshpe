package fshpe.planning

trait Method[S] {

  def decompositions(state: S): LazyList[TaskNetwork[S]]
}

object Method {

  def apply[S](decomposition: TaskNetwork[S]): Method[S] = new Method[S] {
    override def decompositions(state: S): LazyList[TaskNetwork[S]] = decomposition #:: LazyList.empty[TaskNetwork[S]]
  }

  def apply[S](precondition: S => Boolean, decomposition: => TaskNetwork[S]): Method[S] = new Method[S] {
    override def decompositions(state: S): LazyList[TaskNetwork[S]] =
      if (precondition(state)) decomposition #:: LazyList.empty[TaskNetwork[S]] else LazyList.empty[TaskNetwork[S]]
  }

  def apply[S](decompositionsF: S => LazyList[TaskNetwork[S]]): Method[S] = new Method[S] {
    override def decompositions(state: S): LazyList[TaskNetwork[S]] = decompositionsF(state)
  }
}