import gradedmonad._
import gradedmonad.instances._
import gradedmonad.implicits._
import gradedmonad.instances.implicits._

final case class MemoryStore[I <: Int](val f: PartialFunction[I, Int]) {
  def apply(ix: I): Option[Int] = f.lift(ix)
}

implicit object MemoryStore extends EffectUpcast[Int, MemoryStore] {
  def empty: MemoryStore[Nothing] = MemoryStore(Map.empty)

  def domainCast[I1 <: Int, I2 <: Int](s: MemoryStore[I1]): MemoryStore[I2] = s match {
    case MemoryStore(f) => MemoryStore({
      case x if f.isDefinedAt(x.asInstanceOf[I1]) => f(x.asInstanceOf[I1])
    })
  }

  def upcast[I1 <: Int, I2 <: Int](s: MemoryStore[I1]): MemoryStore[I1 | I2] = domainCast(s)

  def addValue[I1 <: Int, I2 <: Int](s: MemoryStore[I1])(ix: I2, v: Int): MemoryStore[I1 | I2] = s match {
    case MemoryStore(f) => MemoryStore({
      case x if x.asInstanceOf[I2] == ix => v
      case x if f.isDefinedAt(x.asInstanceOf[I1]) => f(x.asInstanceOf[I1])
    })
  }
}

type GradedMemoryState[I <: Int, X] = GradedState[Int, MemoryStore, I, X]

def getMemoryStore[I <: Int](ix: I): GradedMemoryState[I, Option[Int]] = GradedState(new GradedStateMapping {
  def apply[I2 <: Int](s: MemoryStore[I2]) = (
    MemoryStore.domainCast[I2, I](s)(ix),
    s.upcast[I]
  )
})

def putMemoryStore[I <: Int](ix: I, v: Int): GradedMemoryState[I, Unit] = GradedState(new GradedStateMapping {
  def apply[I2 <: Int](s: MemoryStore[I2]) = (
    (),
    MemoryStore.addValue(s)(ix, v)
  )
})

/**
def sampleGetPuts: GradedMemoryState[1 | 20 | 5, Option[Int]] =
  for (
    () <- putMemoryStore[1](1, 10);
    x <- getMemoryStore[5](5);
    () <- putMemoryStore[20](20, 3);
    () <- putMemoryStore[5](5, 8)
  ) yield x
*/
