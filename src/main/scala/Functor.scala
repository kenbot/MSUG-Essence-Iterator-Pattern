
trait Functor[M[_]] {
  def fmap[A, B](f: A => B, ma: M[A]): M[B]
}
