object Entities {
  case class User(name: String, age: Int, email: String)

  case class Feed(user: User, posts: List[String])
}
