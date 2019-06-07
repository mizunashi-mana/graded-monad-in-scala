package gradedmonad

object implicits
  extends Functor.ToFunctorOps
  with GradedMonad.ToGradedMonadOps
  with EffectUpcast.ToEffectUpcastOps
