package gradedmonad
package laws

import gradedmonad._
import gradedmonad.implicits._

trait GradedMonadLaws[B, T[_ <: B, _]] {
  implicit def T: GradedMonad[B, T]

  def gradedMonadIdentity[E <: B, X](m: T[E, X]): IsEq[T[E, X]] =
    m.upcast <-> m

  def gradedMonadComposition[E1 <: B, E2 <: B, E3 <: B, X](m: T[E1, X]): IsEq[T[E1 | E2 | E3, X]] =
    m.upcast[E2].upcast[E3] <-> m.upcast[E2 | E3]

  def gradedMonadAssociativity[E1 <: B, E2 <: B, E3 <: B, X](m: T[E1, T[E2, T[E3, X]]]): IsEq[T[E1 | E2 | E3, X]] =
    m.flatten.flatten <-> m.map(_.flatten).flatten

  def gradedMonadLeftIdentity[E <: B, X](m: T[E, X]): IsEq[T[E, X]] =
    GradedMonad.gradedPure(m).flatten <-> m

  def gradedMonadRightIdentity[E <: B, X](m: T[E, X]): IsEq[T[E, X]] =
    m.map(GradedMonad.gradedPure(_)).flatten <-> m
}
