import Validated.Valid
import cats.Applicative

Applicative[Validated].map2(Valid(1), Valid(2))((a, b) => a + b)

