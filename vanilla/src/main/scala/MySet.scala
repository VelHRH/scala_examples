package playground



object Test extends App {
  // we can add MySet object for more convenient use
  val set: MySet[Int] = new NonEmptySet(1, new NonEmptySet(2, new NonEmptySet(3, new EmptySet)))
  set.map(x => x * 10) - 20 foreach println
  println(set(4))
  val set2: MySet[Int] = new NonEmptySet(1, new NonEmptySet(4, new NonEmptySet(3, new EmptySet)))
  set -- set2 foreach println
}

trait MySet[A] extends (A => Boolean) {
  def apply(elem: A): Boolean =
    contains(elem)

  def contains(elem: A): Boolean

  def +(elem: A): MySet[A]

  def ++(anotherSet: MySet[A]): MySet[A] // union

  def map[B](f: A => B): MySet[B]

  def flatMap[B](f: A => MySet[B]): MySet[B]

  def filter(predicate: A => Boolean): MySet[A]

  def foreach(f: A => Unit): Unit

  def -(elem: A): MySet[A]

  def &(set: MySet[A]): MySet[A] // intersection

  def --(set: MySet[A]): MySet[A] // difference
}

class EmptySet[A] extends MySet[A] {
  def contains(elem: A): Boolean = false

  def +(elem: A): MySet[A] = new NonEmptySet[A](elem, this)

  def ++(anotherSet: MySet[A]): MySet[A] = anotherSet

  def map[B](f: A => B): MySet[B] = new EmptySet[B]

  def flatMap[B](f: A => MySet[B]): MySet[B] = new EmptySet[B]

  def filter(predicate: A => Boolean): MySet[A] = this

  def foreach(f: A => Unit): Unit = ()

  def -(elem: A): MySet[A] = EmptySet[A]

  def &(set: MySet[A]): MySet[A] = EmptySet[A]

  def --(set: MySet[A]): MySet[A] = EmptySet[A]
}

class NonEmptySet[A](head: A, tail: MySet[A]) extends MySet[A] {
  def contains(elem: A): Boolean = head == elem || tail.contains(elem)

  def +(elem: A): MySet[A] =
    if (this.contains(elem)) this
    else new NonEmptySet[A](elem, this)

  def ++(anotherSet: MySet[A]): MySet[A] = tail.++(anotherSet).+(head)

  def map[B](f: A => B): MySet[B] = tail.map(f) + f(head)

  def flatMap[B](f: A => MySet[B]): MySet[B] = tail.flatMap(f) ++ f(head)

  def filter(predicate: A => Boolean): MySet[A] =
    if (predicate(head)) tail.filter(predicate) + head
    else tail.filter(predicate)

  def foreach(f: A => Unit): Unit = {
    f(head)
    tail.foreach(f)
  }

  def -(elem: A): MySet[A] =
    if (head == elem) tail
    else tail - elem + head

  def &(set: MySet[A]): MySet[A] = set.filter(elem => this.contains(elem))

  def --(set: MySet[A]): MySet[A] =
    set.filter(elem => !this.contains(elem)) ++ this.filter(elem => !set.contains((elem)))
}






