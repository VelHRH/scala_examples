package playground

trait MyEither[+L, +R] {
  def flatMap[LL >: L, B](f: R => MyEither[LL, B]): MyEither[LL, B]
  def isRight: Boolean
  def isLeft: Boolean
}

case class MyRight[+R](value: R) extends MyEither[Nothing, R] {
  def flatMap[LL >: Nothing, B](f: R => MyEither[LL, B]): MyEither[LL, B] = f(value)
  def isRight = true
  def isLeft = false
}

case class MyLeft[+L](value: L) extends MyEither[L, Nothing] {
  def flatMap[LL >: L, B](f: Nothing => MyEither[LL, B]): MyEither[LL, B] = this
  def isRight = false
  def isLeft = true
}