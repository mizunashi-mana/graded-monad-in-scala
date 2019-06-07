package gradedmonad
package instances

object implicits
  extends GradedState.ToGradedStateOps
  with GradedTry.ToGradedTryOps

