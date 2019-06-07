package gradedmonad
package instances

final case class GradedState[B, S[_ <: B], E <: B, X](val f: GradedStateMapping[B, S, E, X]) {
  def apply[ME <: B](s: S[ME]): (X, S[E | ME]) = f(s)
}

trait GradedStateMapping[B, S[_ <: B], E <: B, X] {
  def apply[ME <: B](s: S[ME]): (X, S[E | ME])
}

object GradedState {
  def gradedPure[B, S[_ <: B], X](x: X)(
    implicit tc: GradedMonad[B, [E <: B, X] => GradedState[B, S, E, X]]
  ): GradedState[B, S, Nothing, X] = tc.gradedPure(x)

  trait ToGradedStateOps {
    implicit def gradedStateOps[B, S[_ <: B]](
      implicit effectUpcast: EffectUpcast[B, S]
    ): GradedMonad[B, [E <: B, X] => GradedState[B, S, E, X]] = new GradedMonad {
      type T[E <: B, X] = GradedState[B, S, E, X]

      def pfunctor[E <: B]: Functor[[X] => T[E, X]] = new Functor {
        type T[X] = GradedState[B, S, E, X]

        def map[X, Y](m: T[X])(f: X => Y): T[Y] = GradedState(new GradedStateMapping {
          def apply[ME <: B](s: S[ME]): (Y, S[E | ME]) = m(s) match {
            case (x, s2) => (f(x), s2)
          }
        })
      }

      def gradedUpcast[E1 <: B, E2 <: B, X](m: T[E1, X]): T[E1 | E2, X] = GradedState(new GradedStateMapping {
        def apply[ME <: B](s: S[ME]): (X, S[E1 | E2 | ME]) = m(s) match {
          case (x, s2) => (x, effectUpcast.upcast[E1 | ME, E2](s2))
        }
      })

      def gradedPure[X](x: X): T[Nothing, X] = GradedState(new GradedStateMapping {
        def apply[ME <: B](s: S[ME]): (X, S[ME]) = (x, s)
      })

      def gradedFlatten[E1 <: B, E2 <: B, X](m: T[E1, T[E2, X]]): T[E1 | E2, X] = GradedState(new GradedStateMapping {
        def apply[ME <: B](s: S[ME]): (X, S[E1 | E2 | ME]) = m(s) match {
          case (m2, s2) => m2(s2)
        }
      })
    }
  }
}
