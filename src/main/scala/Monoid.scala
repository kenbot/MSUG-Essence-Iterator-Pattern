// You can think of it as "Accumulator" or "Appender"...
// The naming debate rages on!
class Monoid[M](val zero: M, val append: (M, M) => M)

// Convenience method for creating Monoids.
// The reason I'm using two parameter lists is so that the type inferred for
// "zero" will carry across to "append".
object Monoid {
  def apply[M](zero: M)(append: (M, M) => M) = new Monoid(zero, append)
}
