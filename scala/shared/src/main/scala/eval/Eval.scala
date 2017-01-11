package eval

class Eval[+A](val run: Context => Either[EvalError, (Context, A)]) {

  def map[B](f: A => B): Eval[B] =
    new Eval(run(_).map { case (newc, a) => (newc, f(a)) })

  def flatMap[B](f: A => Eval[B]): Eval[B] = new Eval(
    run(_).flatMap { case (newc, a) =>
      f(a).run(newc)
    })

  def void: Eval[Unit] = this map (_ => ())

}

object Eval {

  def pure[A](a: A) = new Eval(c => Right((c, a)))

  def error(error: EvalError): Eval[Nothing] = new Eval(_ => Left(error))

//  def get: Eval[Context] =

}
