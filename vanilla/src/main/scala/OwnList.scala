package playground

class Animal(val legs: Int)

class Cat(override val legs: Int) extends Animal(legs)

object OwnList extends App {
  val animal1 = new Cat(5)
  val animal2 = new Cat(2)
  val listAnimals: MyList[Cat] = new Cons(animal1, new Cons(animal2, EmptyList))
  val doubleLegsTransformer: MyTransformer[Animal, Int] = new MyTransformer[Animal, Int] {
    override def transform(input: Animal): Int = input.legs * 2
  }
  println(listAnimals.map(doubleLegsTransformer).allElements)
}

abstract class MyList[+T] {
  def head: T
  def tail: MyList[T]
  def isEmpty: Boolean
  def add[U >: T](element: U): MyList[U]
  def allElements: String
  def map[U](transformer: MyTransformer[T, U]): MyList[U]
}

object EmptyList extends MyList[Nothing] {
  override def head: Nothing = throw new NoSuchElementException()
  override def tail: MyList[Nothing] = throw new NoSuchElementException()
  override def isEmpty: Boolean = true
  override def add[U >: Nothing](element: U): MyList[U] = new Cons(element, EmptyList)
  override def allElements: String = ""
  override def map[U](transformer: MyTransformer[Nothing, U]): MyList[U] = EmptyList
}

class Cons[T](h: T, t: MyList[T]) extends MyList[T] {
  override def head: T = h
  override def tail: MyList[T] = t
  override def isEmpty: Boolean = false
  override def add[U >: T](element: U): MyList[U] = new Cons(element, this)
  override def allElements: String = {
    if (this.isEmpty) ""
    else h.toString + " " + t.allElements
  }

  override def map[U](transformer: MyTransformer[T, U]): MyList[U] = {
    new Cons[U](transformer.transform(h), t.map(transformer))
  }
}

// if we don't put -, transformer of Animal won't work for list of Cat
trait MyTransformer[-A, B] {
  def transform(input: A): B
}