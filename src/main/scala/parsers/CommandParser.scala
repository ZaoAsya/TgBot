package parsers

import commands.Commands

import scala.util.{Success, Try}


object CommandParser {
  def parse(msg: String, privileges: String, userID: Int): Try[String] = {
    val cmd = Commands(userID)
    val name = Try(("/[^ ]+".r findAllIn msg).group(0))
    val message = msg.replace(name.get + " ", "").replace("((", "#$%").replace("))", "%$#")
    lazy val cmdMap = Map(
      "/create_poll" -> Try(ParserCombinators.parseAll(ParserCombinators.create_poll, message).get).map(p =>
        cmd.createPoll(p._1, p._2, p._3, p._4, p._5)),
      "/delete_poll" -> Try(ParserCombinators.parseAll(ParserCombinators.id_number, message).get)
        .map(p => cmd.deletePoll(p)),
      "/start_poll" -> Try(ParserCombinators.parseAll(ParserCombinators.id_number, message).get)
        .map(p => cmd.startPoll(p)),
      "/stop_poll" -> Try(ParserCombinators.parseAll(ParserCombinators.id_number, message).get)
        .map(p => cmd.stopPoll(p)),
      "/begin" -> Try(ParserCombinators.parseAll(ParserCombinators.id_number, message).get)
        .map(p => cmd.begin(p)),
      "/add_question" -> {
        Try(ParserCombinators.parseAll(ParserCombinators.add_question, message).get)
        .map(p => cmd.addQuestion(p._1, p._2, p._3))},
      "/delete_question" -> Try(ParserCombinators.parseAll(ParserCombinators.id_number, message).get)
        .map(p => cmd.deleteQuestion(p)),
      "/answer" -> Try(ParserCombinators.parseAll(ParserCombinators.answer, message).get)
        .map(p => cmd.answer(p._1, p._2)),
      "/result" -> Try(ParserCombinators.parseAll(ParserCombinators.id_number, message).get)
        .map(p => cmd.pollResult(p)),
      "/view" -> Success(cmd.view),
      "/list" -> Success(cmd.pollList),
      "/end" -> Success(cmd.end)
    )

    name.map(n => privileges match {
      case "Administrator" => cmdMap(n)
      case "User" => n match {
        case "/create_poll" | "/delete_poll" | "/start_poll"
             | "/stop_poll" | "/add_question" | "/delete_question" =>
          Success("You don't have such level of privileges")
        case str => cmdMap(str)
      }
    }).getOrElse(Success("Unrecognised command! Say what!?"))
  }
}