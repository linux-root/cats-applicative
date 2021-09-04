import cats.Applicative

sealed trait Validated[+A]

object Validated {
  case class Valid[+A](x: A) extends Validated[A]

  case class Invalid(errors: List[String]) extends Validated[Nothing]

  implicit val applicative: Applicative[Validated] = new Applicative[Validated] {
    override def pure[A](x: A): Validated[A] = Valid(x)

    override def ap[A, B](ff: Validated[A => B])(fa: Validated[A]): Validated[B] =
      (ff, fa) match {
        case (Valid(f), Valid(x)) => Valid(f(x))
        case (Invalid(errorsX), Valid(_)) => Invalid(errorsX)
        case (Invalid(errorsX), Invalid(errorsY)) => Invalid(errorsX ++ errorsY)
        case (Valid(_), Invalid(errorsY)) => Invalid(errorsY)
      }

    override def map2[A, B, C](fa: Validated[A], fb: Validated[B])(f: (A, B) => C): Validated[C] = {
      val pured: Validated[A => B => C] = pure(f.curried) // everytime ap is called, the chain A => B => C => D =>... get shorten. That where 'Applicative' word comes from
      val x: Validated[B => C] = ap(pured)(fa) // apply fa
      ap(x)(fb) // apply fb
    }

    def tupled[A, B](fa: Validated[A], fb: Validated[B]): Validated[(A, B)] = {
      val f : A => B => (A, B) = a => b => (a , b)
      ap(ap(pure(f))(fa))(fb)
    }
  }
}