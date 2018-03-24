package interaction
import scala.util.parsing.combinator._


class CommandsParser extends RegexParsers {
  def create_p : Parser[String] = "/" ~> "create_poll" ^^ (s => s)
  def list_p : Parser[String] = "/" ~> "list" ^^ (s => s)
  def delete_p : Parser[String] = "/" ~> "delete_poll" ^^ (s => s)
  def start_p : Parser[String] = "/" ~> "start_poll" ^^ (s => s)
  def stop_p : Parser[String] = "/" ~> "stop_poll" ^^ (s => s)
  def result_p : Parser[String] = "/" ~> "result" ^^ (s => s)
  def begin_p : Parser[String] = "/" ~> "begin" ^^ (s => s)
  def end_p : Parser[String] = "/" ~> "end" ^^ (s => s)
  def view_p : Parser[String] = "/" ~> "view" ^^ (s => s)
  def add_question_p : Parser[String] = "/" ~> "add_question" ^^ (s => s)
  def poll_name : Parser[String] = "(" ~> ".*?\\)*".r <~ ")" ^^ (str => str)
  def anonymous : Parser[Boolean] = "(" ~> "(yes)|(no)".r <~ ")" ^^ (expr => expr == "yes")
  def view_type : Parser[String] = "(" ~> "(afterstop)|(continuous)".r <~ ")" ^^ (expr => expr)
  def time_start : Parser[String] = "(" ~> "\\d{2}:\\d{2}:\\d{2} \\d{2}:\\d{2}:\\d{2}".r <~ ")" ^^ (expr => expr)
  def time_stop : Parser[String] = "(" ~> "\\d{2}:\\d{2}:\\d{2} \\d{2}:\\d{2}:\\d{2}" <~ ")" ^^ (expr => expr)
  def id : Parser[Int] = "(" ~> "\\d+?" <~ ")" ^^ (expr => expr.toInt)
  def question : Parser[String] = "(" ~> ".*?\\)*".r <~ ")" ^^ (expr => expr)
  def question_type : Parser[String] = "(" ~> "(open)|(choice)|(multi)".r <~ ")" ^^ (expr => expr)
  def answer : Parser[String] = "\n(\t| )*(.*)\n??".r ^^ (expr => expr)

  def create_poll : Parser[(String, String, List[Boolean], List[String], List[String], List[String])] =
    create_p ~ poll_name ~ rep(anonymous) ~ rep(view_type) ~ rep(time_start) ~ rep(time_stop) ^^
      (expr => (expr._1._1._1._1._1, expr._1._1._1._1._2, expr._1._1._1._2, expr._1._1._2, expr._1._2, expr._2))
  def list : Parser[String] = list_p ^^ (expr => expr)
  def delete_poll : Parser[(String, Int)] = delete_p ~ id ^^ (expr => (expr._1, expr._2))
  def start_poll : Parser[(String, Int)] = start_p ~ id ^^ (expr => (expr._1, expr._2))
  def stop_poll : Parser[(String, Int)] = stop_p ~ id ^^ (expr => (expr._1, expr._2))
  def result : Parser[(String, Int)] = result_p ~ id ^^ (expr => (expr._1, expr._2))
  def begin : Parser[(String, Int)] = begin_p ~ id ^^ (expr => (expr._1, expr._2))
  def end : Parser[String] = end_p ^^ (expr => expr)
  def view : Parser[String] = view_p ^^ (expr => expr)
  def add_question : Parser[(String, String, List[String], List[String])] =
    add_question_p ~ question ~ rep(question_type) ~ rep(answer) ^^
      (expr => (expr._1._1._1, expr._1._1._2, expr._1._2, expr._2))
}
