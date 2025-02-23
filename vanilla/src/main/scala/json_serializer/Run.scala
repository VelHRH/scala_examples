import Entities.*
import JSONStringifiers.*
import JSONOps.JSONOps

object Run extends App {
  val data = JSONStringifierForObject(Map(
    "user" -> JSONStringifierForString("Daniel"),
    "posts" -> JSONStringifierForList(List(
      JSONStringifierForString("Scala Rocks!"),
      JSONStringifierForInt(453)
    ))
  ))

  println(data.stringify)

  // Now it works, but only with types like Map, String, List. Plus we need to think of which class wraps param we want to stringify
  // What we want first: 453.jsonStringify. Of course, it's not right to add methods right into classes. We should use implicit pimping
  // Let's create Type Class that can work with any value converting in to stringifier structure like above

  println(123.jsonStringify) // Int doesn't have jsonStringify so it searches for appropriate implicit class
  // It works! Now we can make the same for any type, for example User("John", 34, "john@rockthejvm.com").jsonStringify


  val john = User("John", 34, "john@rockthejvm.com")
  val feed = Feed(john, List("News", "Memes"))
  println(feed.jsonStringify)
}

