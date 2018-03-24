package commands

import scala.util.Try
import exceptions._


class Command(name : Try[String], params : Try[List[String]], answers : Try[List[String]]) {
  def execute() : String = {
    this.name.getOrElse() match {
      case "/create_poll" => General.createPoll(this.params);
      case "/list" => General.pollList()
      case "/delete_poll" => General.deletePoll(this.params)
      case "/start_poll" => General.startPoll(this.params)
      case "/stop_poll" => General.stopPoll(this.params)
      case "/result" => General.pollResult(this.params)
      case "/add_question" => General.addQuestion(this.params, answers)
      case "/begin" => General.begin(this.params)
      case "/end" => General.end
      case "/view" => General.view
//      case "/delete_question" => General.pollResult(this.params.head)
//      case "/answer" => General.pollResult(this.params.head)
      case _ => "Unrecognised command! Say what!?"
    }
  }
}