package gradedmonad
package instances

enum GradedTry[B, S[_ <: B], E <: B, X] {
  case GradedSuccess(val result: X)
  case GradedFailure(val error: S[E])
}

object GradedTry {
  trait ToGradedTryOps {
    implicit def gradedTryFunctorOps[B, S[_ <: B], E <: B]: Functor[[X] => GradedTry[B, S, E, X]] = new Functor {
      type T[X] = GradedTry[B, S, E, X]

      def map[X, Y](m: T[X])(f: X => Y): T[Y] = m match {
        case GradedSuccess(x) => GradedSuccess(f(x))
        case GradedFailure(e) => GradedFailure(e)
      }
    }

    implicit def gradedTryOps[B, S[_ <: B]](
      implicit effectUpcast: EffectUpcast[B, S]
    ): GradedMonad[B, [E <: B, X] => GradedTry[B, S, E, X]] = new GradedMonad {
      type T[E <: B, X] = GradedTry[B, S, E, X]

      def pfunctor[E <: B]: Functor[[X] => T[E, X]] = gradedTryFunctorOps

      def gradedUpcast[E1 <: B, E2 <: B, X](m: T[E1, X]): T[E1 | E2, X] = m match {
        case GradedSuccess(x) => GradedSuccess(x)
        case GradedFailure(e) => GradedFailure(effectUpcast.upcast[E1, E1 | E2](e))
      }

      def gradedPure[X](x: X): T[Nothing, X] = GradedSuccess(x)

      def gradedFlatten[E1 <: B, E2 <: B, X](m: T[E1, T[E2, X]]): T[E1 | E2, X] = m match {
        case GradedSuccess(m2) => m2 match {
          case GradedSuccess(x) => GradedSuccess(x)
          case GradedFailure(e) => GradedFailure(effectUpcast.upcast[E2, E1 | E2](e))
        }
        case GradedFailure(e) => GradedFailure(effectUpcast.upcast[E1, E1 | E2](e))
      }
    }
  }
}
