import Converters.ConverterToJSONStringifier

object JSONOps {
  implicit class JSONOps[T](value: T) {
    def jsonStringify(implicit converter: ConverterToJSONStringifier[T]): String =
      val jsonStringifier = converter.convert(value)
      jsonStringifier.stringify
  }
}
