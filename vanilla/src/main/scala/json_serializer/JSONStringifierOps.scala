import Converters.ConverterToJSONStringifier
import JSONStringifiers.JSONStringifier

object JSONStringifierOps {
  implicit class JSONStringifierOps[T](value: T) {
    def convert(implicit converter: ConverterToJSONStringifier[T]): JSONStringifier =
      converter.convert(value)
  }
}
