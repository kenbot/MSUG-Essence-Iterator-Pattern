

object State {
  type apply[A] = {
    type toArg1[B] = State[A, B]
    type toArg2[B] = State[B, A]
  }
  
  implicit def stateMonad[S] = new Monad[State.apply[S]#toArg1] { 
    def pure[A](a: A): State[S, A] = State(s => (a, s))
    def bind[A, B](ma: State[S, A])(f: A => State[S, B]) = State[S, B] {s => 
        val (a, s2) = ma runState s
        f(a) runState s2
    }
  }
}

case class State[S, +A](runState: S => (A, S))