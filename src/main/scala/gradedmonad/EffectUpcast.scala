package gradedmonad

trait EffectUpcast[B, S[_ <: B]] {
  def upcast[E1 <: B, E2 <: B](se: S[E1]): S[E1 | E2]
}

object EffectUpcast {
  def apply[B, S[_ <: B]](implicit tc: EffectUpcast[B, S]): EffectUpcast[B, S] = tc

  trait ToEffectUpcastOps {
    implicit final class toEffectUpcastOps[B, S[_ <: B], E <: B](private val se: S[E]) {
      def upcast[E2 <: B](implicit tc: EffectUpcast[B, S]): S[E | E2] = tc.upcast[E, E2](se)
    }
  }
}
