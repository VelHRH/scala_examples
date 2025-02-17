package playground

trait MyTry[+A] {
  def flatMap[B](f: A => MyTry[B]): MyTry[B]
}
case class Success[+A](value: A) extends MyTry[A] {
  def flatMap[B](f: A => MyTry[B]): MyTry[B] = {
    try {
      f(value)
    } catch {
      case e: Throwable => Fail(e)
    }
  }
}
case class Fail(e: Throwable) extends MyTry[Nothing] {
  def flatMap[B](f: Nothing => MyTry[B]): MyTry[B] = this
}
object MyTry {
  def apply[A](value: => A): MyTry[A] = {
    try {
      Success(value) // if we pass something like 3/0, it is evaluated only here
    } catch {
      case e: Throwable => Fail(e)
    }
  }
}