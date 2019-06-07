package gradedmonad
package laws

import gradedmonad._
import gradedmonad.implicits._

trait FunctorLaws[F[_]] {
  implicit def T: Functor[F]

  def functorIdentity[X](fx: F[X]): IsEq[F[X]] =
    fx.map(identity) <-> fx

  def functorComposition[X, Y, Z](fx: F[X])(f: X => Y, g: Y => Z): IsEq[F[Z]] =
    fx.map(f.andThen(g)) <-> fx.map(f).map(g)
}
