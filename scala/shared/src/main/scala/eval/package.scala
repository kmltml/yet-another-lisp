package object eval {

  implicit class RightBiasedEither[L, R](private val e: Either[L, R]) {

    def map[A](f: R => A): Either[L, A] = e.right.map(f)

    def flatMap[A](f: R => Either[L, A]): Either[L, A] = e.right.flatMap(f)

  }

}
