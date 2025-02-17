package playground

import scala.collection.mutable
import scala.util.Random

class SimpleContainer {
  private var value: Int = 0

  def isEmpty: Boolean = value == 0

  def set(newValue: Int) = value = newValue

  def get = {
    val result = value
    value = 0
    result
  }
}

class Producer(id: Int, queue: mutable.Queue[Int], capacity: Int) extends Thread {
  override def run(): Unit = {
    val random = new Random()
    var i = 0
    while (true) {
      queue.synchronized {
        while (queue.size == capacity) {
          println(s"[producer $id] Queue is full...")
          queue.wait()
        }
        println(s"[producer $id] Producing ${queue.enqueue(i)}")
        i += 1
        queue.notifyAll()
      }
      Thread.sleep(random.nextInt(500))
    }
  }
}

class Consumer(id: Int, queue: mutable.Queue[Int]) extends Thread {
  override def run(): Unit = {
    val random = new Random()
    while (true) {
      queue.synchronized {
        while (queue.isEmpty) {
          println(s"[consumer $id] Queue is empty...")
          queue.wait()
        }
        println(s"[consumer $id] Consuming ${queue.dequeue()}")
        queue.notifyAll()
      }
      Thread.sleep(random.nextInt(500))
    }
  }
}

object ProducerConsumer {
  def level1(): Unit = {
    val capacity = 3
    val queue = new mutable.Queue[Int](capacity)

    val consumer = new Thread(() => {
      val random = new Random()
      while (true) {
        queue.synchronized {
          if (queue.isEmpty) {
            println("[consumer] Queue is empty...")
            queue.wait()
          }
          println(s"[consumer] Consuming ${queue.dequeue()}")
          queue.notify()
        }
        Thread.sleep(random.nextInt(1000))
      }
    })

    val producer = new Thread(() => {
      val random = new Random()
      var i = 0
      while (true) {
        queue.synchronized {
          if (queue.size == capacity) {
            println("[producer] Queue is full...")
            queue.wait()
          }
          println(s"[producer] Producing ${queue.enqueue(i)}")
          i += 1
          queue.notify()
        }
        Thread.sleep(random.nextInt(500))
      }
    })

    consumer.start()
    producer.start()
  }

  def level2(prods: Int, cons: Int): Unit = {
    val capacity = 3
    val queue = new mutable.Queue[Int](capacity)

    (1 to cons).foreach((i) => new Consumer(i, queue).start())
    (1 to prods).foreach((i) => new Producer(i, queue, capacity).start())
  }
}
