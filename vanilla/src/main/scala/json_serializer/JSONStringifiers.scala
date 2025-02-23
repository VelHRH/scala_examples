object JSONStringifiers {
  sealed trait JSONStringifier {
    def stringify: String
  }

  final case class JSONStringifierForString(value: String) extends JSONStringifier {
    def stringify: String =
      "\"" + value + "\""
  }

  final case class JSONStringifierForInt(value: Int) extends JSONStringifier {
    def stringify: String = value.toString
  }

  final case class JSONStringifierForList(values: List[JSONStringifier]) extends JSONStringifier {
    def stringify: String = values.map(_.stringify).mkString("[", ",", "]")
  }

  final case class JSONStringifierForObject(values: Map[String, JSONStringifier]) extends JSONStringifier {
    def stringify: String = values.map {
        case (key, value) => "\"" + key + "\":" + value.stringify
      }
      .mkString("{", ",", "}")

  }
}
