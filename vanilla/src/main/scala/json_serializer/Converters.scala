import JSONStringifiers.*
import JSONStringifierOps.JSONStringifierOps
import Entities.*

object Converters {
  trait ConverterToJSONStringifier[T] {
    def convert(value: T): JSONStringifier
  }

  implicit object ConverterOfStringToJSONStringifier extends ConverterToJSONStringifier[String] {
    def convert(value: String): JSONStringifier = JSONStringifierForString(value)
  }

  implicit object ConverterOfIntToJSONStringifier extends ConverterToJSONStringifier[Int] {
    def convert(value: Int): JSONStringifier = JSONStringifierForInt(value)
  }

  implicit object ConverterOfUserToJSONStringifier extends ConverterToJSONStringifier[User] {
    def convert(user: User): JSONStringifier = JSONStringifierForObject(
      Map(
        "name" -> JSONStringifierForString(user.name),
        "age" -> JSONStringifierForInt(user.age),
        "email" -> JSONStringifierForString(user.email),
      )
    )
  }

  implicit object ConverterOfFeedToJSONStringifier extends ConverterToJSONStringifier[Feed] {
    def convert(feed: Feed): JSONStringifier = JSONStringifierForObject(
      Map(
        "user" -> feed.user.convert, // User has no .convert, so compilers searches in implicit imported JSONStringifierOps
        "posts" -> JSONStringifierForList(feed.posts.map(JSONStringifierForString(_)))
      )
    )
  }
}
