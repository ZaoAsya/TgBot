package interaction
import scala.util.parsing.combinator._


class ParamsParser extends RegexParsers {
  def anonymous : Parser[Boolean] ="(yes)|(no)".r ^^ (expr => expr == "yes")
  def view_type : Parser[String] = "(afterstop)|(continuous)".r ^^ (expr => expr)
  def time : Parser[String] ="\\d{2}:\\d{2}:\\d{2} \\d{2}:\\d{2}:\\d{2}".r ^^ (expr => expr)
  def qtype : Parser[String] ="(open)|(choice)|(multi)".r ^^ (expr => expr)
}
