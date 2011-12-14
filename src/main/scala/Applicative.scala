

// Convenience methods to lift a computation into applicative-land with 0, 1, 2 or 3 arguments.
object Applicative {
  private def applicM[M[_]](implicit am: Applicative[M]) = am
  
  def apply[M[_]: Applicative, Result](a: Result): M[Result] = applicM.pure(a)

  def apply[M[_]: Applicative, A, Result](f: A => Result, ma: M[A]): M[Result] = {
    applicM.applic(apply(f), ma)
  } 
  
  def apply[M[_]: Applicative, A, B, Result](f: (A, B) => Result, ma: M[A], mb: M[B]): M[Result] = {
    applicM.applic(apply(f.curried(_: A), ma), mb)
  }

  def apply[M[_]: Applicative, A, B, C, Result](f: (A, B, C) => Result, ma: M[A], mb: M[B], mc: M[C]): M[Result] = {
    applicM.applic(apply(f.curried(_: A)(_: B), ma, mb), mc)
  }
}

trait Applicative[M[_]] extends Functor[M] {
  def pure[A](a: A): M[A]
  def applic[A,B](mf: M[A => B], ma: M[A]): M[B]
  def fmap[A,B](f: A => B, ma: M[A]): M[B] = applic(pure(f), ma)
}
