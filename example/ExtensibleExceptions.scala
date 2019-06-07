import gradedmonad._
import gradedmonad.instances._
import gradedmonad.implicits._
import gradedmonad.instances.implicits._

sealed abstract class CustomException
final case class Exception1() extends CustomException
final case class Exception2() extends CustomException
final case class Exception3() extends CustomException

final case class Exc[E <: CustomException](val exc: E)

implicit object Exc extends EffectUpcast[CustomException, Exc] {
  def upcast[E1 <: CustomException, E2 <: CustomException](e: Exc[E1]): Exc[E1 | E2] = e match {
    case Exc(e) => Exc(e)
  }
}

type GradedExcTry[E <: CustomException, X] = GradedTry[CustomException, Exc, E, X]

def fromEither[E <: CustomException, X](r: Either[E, X]): GradedExcTry[E, X] = r match {
  case Left(e)  => GradedTry.GradedFailure(Exc(e))
  case Right(v) => GradedTry.GradedSuccess(v)
}

/**
 *
 * scala> sampleTry(10)
 * val res: GradedExcTry[Exception1 | Exception2, Boolean] = GradedSuccess(false)
 * scala> sampleTry(5)
 * val res: GradedExcTry[Exception1 | Exception2, Boolean] = GradedFailure(Exc(Exception2()))
 *
 */
def sampleTry(v: Int): GradedExcTry[Exception1 | Exception2, Boolean] =
  for {
    v <- fromEither(if v < 1
      then Left(Exception1())
      else Right(v / 2)
    )
    v <- fromEither(if v % 2 == 0
      then Left(Exception2())
      else Right(v / 2)
    )
  } yield v > 10
