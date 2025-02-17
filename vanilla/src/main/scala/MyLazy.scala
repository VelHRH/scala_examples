package playground

trait MyLazy[+A] {
  def flatMap[B](f: (=> A) => MyLazy[B]): MyLazy[B]
}

class LazyVal[+A](value: => A) extends MyLazy[A] {
  private lazy val internalValue = value // call by need to always evaluate once
  def flatMap[B](f: (=> A) => MyLazy[B]): MyLazy[B] = f(internalValue)
}

object MyLazy {
  def apply[A](value: => A): MyLazy[A] = {
    new LazyVal(value)
  }
}