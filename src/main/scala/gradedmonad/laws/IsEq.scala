package gradedmonad
package laws

final case class IsEq[A](lhs: A, rhs: A)

implicit final class IsEqArrow[A](private val lhs: A) extends AnyVal {
  def <->(rhs: A): IsEq[A] = IsEq(lhs, rhs)
}
