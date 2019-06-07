package gradedmonad

trait Functor[F[_]] { self =>
  def map[X, Y](fa: F[X])(f: X => Y): F[Y]
}

object Functor {
  def apply[F[_]](implicit tc: Functor[F]): Functor[F] = tc

  trait ToFunctorOps {
    implicit final class toFunctorOps[F[_], X](private val fx: F[X]) {
      def map[Y](f: X => Y)(implicit tc: Functor[F]): F[Y] = tc.map(fx)(f)
    }
  }
}
