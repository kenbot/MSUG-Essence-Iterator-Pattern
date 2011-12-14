
object Traverse {
  def apply[Context[_]: Applicative, T[_]: Traverse, A, B](traversable: T[A])(mapAndLift: A => Context[B]): Context[T[B]] = {
    implicitly[Traverse[T]].traverse(traversable, mapAndLift)
  }
}

trait Traverse[T[_]] {
  def traverse[Context[_]: Applicative, A, B](traversable: T[A], mapAndLift: A => Context[B]): Context[T[B]]
}
