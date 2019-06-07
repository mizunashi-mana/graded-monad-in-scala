package gradedmonad

/**
 * An instance of GradedMonad should be satisfied the GradedMonadLaws
 */
trait GradedMonad[B, T[_ <: B, _]] { self =>
  def pfunctor[E <: B]: Functor[[X] => T[E, X]]

  def gradedUpcast[E1 <: B, E2 <: B, X](m: T[E1, X]): T[E1 | E2, X]

  def gradedPure[X](x: X): T[Nothing, X]

  def gradedFlatten[E1 <: B, E2 <: B, X](m: T[E1, T[E2, X]]): T[E1 | E2, X]

  def gradedFlatMap[E1 <: B, E2 <: B, X, Y](m: T[E1, X])(f: X => T[E2, Y]): T[E1 | E2, Y] =
    gradedFlatten[E1, E2, Y](pfunctor.map(m)(f))
}

object GradedMonad {
  def apply[B, T[_, _]](implicit tc: GradedMonad[B, T]): GradedMonad[B, T] = tc

  def gradedPure[B, T[_, _], X](x: X)(implicit tc: GradedMonad[B, T]): T[Nothing, X] = tc.gradedPure(x)

  trait ToGradedMonadOps {
    implicit final class toGradedMonadOps[B, T[_ <: B, _], E <: B, X](
      private val tex: T[E, X]
    )(implicit tc: GradedMonad[B, T]) {
      def map[Y](f: X => Y): T[E, Y] = tc.pfunctor.map(tex)(f)

      def flatMap[E2 <: B, Y](f: X => T[E2, Y]): T[E | E2, Y] = tc.gradedFlatMap(tex)(f)

      def withFilter(p: X => Boolean): T[E, X] = tc.pfunctor.map(tex)(
        x => if p(x)
          then x
          else throw new RuntimeException("Pattern match failed: GradedMonad.withFilter")
      )

      def upcast[E2 <: B]: T[E | E2, X] = tc.gradedUpcast[E, E2, X](tex)
    }

    implicit final class toGradedMonadFlattenOps[B, T[_ <: B, _], E1 <: B, E2 <: B, X](
      private val teex: T[E1, T[E2, X]]
    )(implicit tc: GradedMonad[B, T]) {
      def flatten: T[E1 | E2, X] = tc.gradedFlatten[E1, E2, X](teex)
    }
  }
}
