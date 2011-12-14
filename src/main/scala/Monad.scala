
trait Monad[M[_]] extends Applicative[M] {
  def pure[A](a: A): M[A]
  def bind[A, B](ma: M[A])(f: A => M[B]): M[B]
  def applic[A, B](mf: M[A => B], ma: M[A]): M[B] = bind(mf)(f => bind(ma)(a => pure(f(a))))
}