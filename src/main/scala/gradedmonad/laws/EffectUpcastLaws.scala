package gradedmonad
package laws

import gradedmonad._
import gradedmonad.implicits._

trait EffectUpcastLaws[B, S[_ <: B]] {
  implicit def T: EffectUpcast[B, S]

  def functorIdentity[E <: B](se: S[E]): IsEq[S[E]] =
    se.upcast <-> se

  def functorComposition[E1 <: B, E2 <: B, E3 <: B](se: S[E1]): IsEq[S[E1 | E2 | E3]] =
    se.upcast[E2].upcast[E3] <-> se.upcast
}
