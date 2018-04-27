package parsers

import commands.Commands

import scala.util.{Success, Try}


object CommandParser {
  def parse(msg: String, privileges: String, userID: Int): Try[String] = {
    val name = Try(("/[^ ]+".r findAllIn msg).group(0))
    val cmd = Commands(userID)
    val parser = new parsers.ParserCombinators()
    if (privileges == "Administrator" && name.isSuccess) {
      val message = msg.replace(name.get + " ", "").replace("((", "#$%").replace("))", "%$#")
      name.get match {
        case "/create_poll" =>
          Try(parser.parseAll(parser.create_poll, message).get).map(p =>
            cmd.createPoll(p._1, p._2, p._3, p._4, p._5))
        case "/delete_poll" => Try(parser.parseAll(parser.id_number, message).get)
          .map(p => cmd.deletePoll(p))
        case "/start_poll" => Try(parser.parseAll(parser.id_number, message).get)
            .map(p => cmd.startPoll(p))
        case "/stop_poll" => Try(parser.parseAll(parser.id_number, message).get)
            .map(p => cmd.stopPoll(p))
        case "/begin" => Try(parser.parseAll(parser.id_number, message).get)
            .map(p => cmd.begin(p))
        case "/delete_question" => Try(parser.parseAll(parser.id_number, message).get)
            .map(p => cmd.deleteQuestion(p))
        case "/answer" => Try(parser.parseAll(parser.id_number, message).get)
            .map(p => cmd.answer(p))
        case "/add_question" => Try(parser.parseAll(parser.add_question, message).get)
            .map( p => cmd.addQuestion(p._1, p._2, p._3))
        case "/result" => Try(parser.parseAll(parser.id_number, message).get)
            .map(p => cmd.pollResult(p))
        case "/end" => Success(cmd.end)
        case "/view" => Success(cmd.view)
        case "/list" => Success(cmd.pollList)
        case _ => Success("Unrecognised command! Say what!?")
      }
    }
    else if (privileges == "User" && name.isSuccess){
      val message = msg.replace(name.get + " ", "").replace("((", "#$%").replace("))", "#$%")
      name.get match {
        case "/create_poll" | "/delete_poll" | "/start_poll"
             | "/stop_poll" | "/add_question" | "/delete_question" =>
          Success("You don't have such level of privileges")
        case "/list" => Success(cmd.pollList)
        case "/end" => Success(cmd.end)
        case "/view" => Success(cmd.view)
        case "/result" => Try(parser.parseAll(parser.id_number, message).get)
            .map(p => cmd.pollResult(p))
        case "/begin" => Try(parser.parseAll(parser.id_number,message).get)
            .map(p => cmd.begin(p))
        case "/answer" => Try(parser.parseAll(parser.id_number, message).get)
            .map(p => cmd.answerDaemon(p))
        case _ => Success("Unrecognised command! Say what!?")
      }
    }
    else Success("Command Sir!")
  }
}