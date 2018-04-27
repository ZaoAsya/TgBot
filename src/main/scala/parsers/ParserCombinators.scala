package parsers
import scala.util.parsing.combinator._
class ParserCombinators extends RegexParsers {
  def id_number : Parser[String] = "(" ~> "\\d+".r <~ ")"

  def question : Parser[String] = "(" ~> ".*?(\\)*.*)*".r <~ ")"
  def question_type : Parser[String] = "(" ~> "(open)|(choice)|(multi)".r <~ ")"
  def answer : Parser[String] = "\n(\t| |)*(.*)\n??".r
  def add_question : Parser[(String, String, List[String])] =
    question ~ rep(question_type) ~ rep(answer)^^
      (expr => {
        val qtype = if (expr._1._2.nonEmpty) expr._1._2.head else "open"
        if (qtype == "open" && expr._2.nonEmpty) Failure
        if (qtype != "open" && expr._2.isEmpty) Failure
        (expr._1._1, qtype, expr._2)})

  def poll_name : Parser[String] = "(" ~> "[^)]+".r <~ ")" ^^ (str => str)
  def anonymous : Parser[Boolean] = "(" ~> "(yes)|(no)".r <~ ")" ^^ (expr => expr == "yes")
  def view_type : Parser[String] = "(" ~> "(afterstop)|(continuous)".r <~ ")"
  def time : Parser[String] = "(" ~> "\\d{2}:\\d{2}:\\d{2} \\d{2}:\\d{2}:\\d{2}".r <~ ")"
  def create_poll : Parser[(String, Boolean, String, String, String)] =
    poll_name ~ rep(anonymous) ~ rep(view_type) ~ rep(time) ~ rep(time) ^^
      (expr => {
        val name = expr._1._1._1._1.replace("#$%", "(").replace("%$#", ")")
        val anonymous = if (expr._1._1._1._2.nonEmpty) expr._1._1._1._2.head else true
        val vew_type = if (expr._1._1._2.nonEmpty) expr._1._1._2.head else "afterstop"
        val time_start = if (expr._1._2.nonEmpty) expr._1._2.head else "null"
        val time_stop = if (expr._2.nonEmpty) expr._2.head else "null"
        (name, anonymous, vew_type, time_start, time_stop)
      })
}
