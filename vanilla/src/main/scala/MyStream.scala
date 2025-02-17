package playground

import scala.annotation.tailrec

object DemoStream extends App {
  val stream = MyStream.from(0)(_ + 5)
  stream.flatMap(x => new NonEmptyStream(x, new NonEmptyStream(x + 1, EmptyStream))) take 10 foreach println
  println(stream.filter(_ > 5).takeAsList(5))
  println(stream takeAsList 10)

  def fibonacci(first: BigInt, second: BigInt): MyStream[BigInt] = {
    new NonEmptyStream(first, fibonacci(second, first + second))
  }

  println(fibonacci(1, 1).takeAsList(10))

  def primes(numbers: MyStream[Int]): MyStream[Int] = {
    new NonEmptyStream(numbers.head, primes(numbers.tail.filter(_ % numbers.head != 0)))
  }

  println(primes(MyStream.from(2)(_ + 1)).takeAsList(10))
}


abstract class MyStream[+A] {
  def isEmpty: Boolean

  def head: A

  def tail: MyStream[A]

  def #::[B >: A](element: B): MyStream[B] 

  def ++[B >: A](anotherStream: => MyStream[B]): MyStream[B]

  def foreach(f: A => Unit): Unit

  def map[B](f: A => B): MyStream[B]

  def flatMap[B](f: A => MyStream[B]): MyStream[B]

  def filter(predicate: A => Boolean): MyStream[A]

  def take(n: Int): MyStream[A]

  def takeAsList(n: Int): List[A]

  @tailrec
  final def toList[B >: A](acc: List[B] = Nil): List[B] = {
    if (this.isEmpty) acc.reverse
    else tail.toList(acc.::(head))
  }
}

object EmptyStream extends MyStream[Nothing] {
  def isEmpty: Boolean = true
  def head: Nothing = throw new NoSuchElementException()
  def tail: MyStream[Nothing] = throw new NoSuchElementException()

  def #::[B >: Nothing](element: B): MyStream[B] = new NonEmptyStream[B](element, this)
  def ++[B >: Nothing](anotherStream: => MyStream[B]): MyStream[B] = anotherStream

  def foreach(f: Nothing => Unit): Unit = ()
  def map[B](f: Nothing => B): MyStream[B] = this
  def flatMap[B](f: Nothing => MyStream[B]): MyStream[B] = this
  def filter(predicate: Nothing => Boolean): MyStream[Nothing] = this

  def take(n: Int): MyStream[Nothing] = this
  def takeAsList(n: Int): List[Nothing] = Nil
}

class NonEmptyStream[+A](hd: A, tl: => MyStream[A]) extends MyStream[A] {
  def isEmpty: Boolean = false
  val head: A = hd
  lazy val tail: MyStream[A] = tl

  def #::[B >: A](element: B): MyStream[B] = new NonEmptyStream(element, this)
  def ++[B >: A](anotherStream: => MyStream[B]): MyStream[B] = new NonEmptyStream(head, tail ++ anotherStream)
  // ++ preserves laziness, as it is passed as call by name param

  def foreach(f: A => Unit): Unit = {
    f(head)
    tail.foreach(f) // NOT lazy
  }

  def map[B](f: A => B): MyStream[B] = new NonEmptyStream(f(head), tail.map(f))  // lazy
  def flatMap[B](f: A => MyStream[B]): MyStream[B] = f(head) ++ tail.flatMap(f) // lazy

  def filter(predicate: A => Boolean): MyStream[A] = {
    if (predicate(head)) new NonEmptyStream(head, tail.filter(predicate))
    else tail.filter(predicate) // lazy
  }

  def take(n: Int): MyStream[A] = {
    if (n <= 0) EmptyStream
    else if (n==1) new NonEmptyStream(head, EmptyStream)
    else new NonEmptyStream(head, tail.take(n-1)) // lazy
  }

  def takeAsList(n: Int): List[A] = take(n).toList()
}

object MyStream {
  def from[A](start: A)(generator: A => A): MyStream[A] =
    new NonEmptyStream(start, MyStream.from(generator(start))(generator))
}


