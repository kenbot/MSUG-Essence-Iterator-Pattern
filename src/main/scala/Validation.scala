
object Validation {
  implicit def validationApplicative: Applicative[Validation] = new Applicative[Validation] {
    def pure[A](a: A): Validation[A] = Success(a)
    def applic[A,B](mf: Validation[A => B], ma: Validation[A]): Validation[B] = (mf, ma) match {
      case (Success(f), Success(a)) => Success(f(a))
      case (Success(f), Failure(errs)) => Failure(errs)
      case (Failure(errs), Success(a)) => Failure(errs)
      case (Failure(errs1), Failure(errs2)) => Failure(errs1 ++ errs2)
    }
  }
}


sealed trait Validation[+A]
case class Success[+A](value: A) extends Validation[A]
case class Failure(errors: List[String]) extends Validation[Nothing]