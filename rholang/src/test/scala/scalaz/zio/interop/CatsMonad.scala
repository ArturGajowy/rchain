package scalaz.zio
package interop

import cats.Monad

//Turns out, zio's instance for cats.Monad is not stacksafe, becasue of a missing suspend in tailRecM
private class CatsMonad[R, E] extends Monad[ZIO[R, E, ?]] {
  override final def pure[A](a: A): ZIO[R, E, A]                          = ZIO.succeed(a)
  override final def map[A, B](fa: ZIO[R, E, A])(f: A => B): ZIO[R, E, B] = fa.map(f)
  override final def flatMap[A, B](fa: ZIO[R, E, A])(f: A => ZIO[R, E, B]): ZIO[R, E, B] =
    fa.flatMap(f)
  override final def tailRecM[A, B](a: A)(f: A => ZIO[R, E, Either[A, B]]): ZIO[R, E, B] =
    ZIO.suspend(f(a)).flatMap {
      case Left(l)  => tailRecM(l)(f)
      case Right(r) => ZIO.succeed(r)
    }
}
