package commands

import java.util.Date

import Repository._
import interaction.ParamsParser
import poll.Poll

import scala.util.Try

object General {
  val parser = new ParamsParser()
  private def getMonth(month: String): String = {
    month match {
      case "Jan" => "01"
      case "Feb" => "02"
      case "Mar" => "03"
      case "Apr" => "04"
      case "May" => "05"
      case "Jun" => "06"
      case "Jul" => "07"
      case "Aug" => "08"
      case "Sep" => "09"
      case "Oct" => "10"
      case "Nov" => "11"
      case "Dec" => "12"
    }
  }

  private def getDateTime(input: String): String = {
    val date = input.split(' ')
    date(3) + ' ' + date(5).drop(2) + ':' + getMonth(date(1)) + ':' + date(2)
  }

  def createPoll(params: Try[List[String]]): String = {
    val name = Try(params.get.head)

    val anonymous: Try[Boolean] = Try(
      if (Try(params.get(1)).isFailure) true
      else parser.parseAll(parser.anonymous, params.get(1)).get
    )

    val viewType: Try[String] = Try(
      if (Try(params.get(2)).isFailure) "afterstop"
      else parser.parseAll(parser.view_type, params.get(2)).get
    )

    val startTime: Try[String] = Try(
      if (Try(params.get(3)).isFailure) getDateTime(new Date().toString)
      else parser.parseAll(parser.time, params.get(3)).get
    )

    val stopTime: Try[String] = Try(
      if (Try(params.get(4)).isFailure) "null"
      else parser.parseAll(parser.time, params.get(4)).get
    )

    if (name.isSuccess && anonymous.isSuccess && viewType.isSuccess &&
        startTime.isSuccess && stopTime.isSuccess)
      createPollDaemon(name.get,
                       anonymous.get,
                       viewType.get,
                       startTime.get,
                       stopTime.get)
    else "can't create a poll"
  }

  private def createPollDaemon(title: String,
                               anonymous: Boolean,
                               viewType: String,
                               startTime: String,
                               stopTime: String): String = {
    val poll = new Poll(title, anonymous, viewType, startTime, stopTime)
    val id = AllPolls.get_id()
    AllPolls.set(id, poll)
    "Success: " + id
  }

  def pollList(): String = {
    if (AllPolls.getAll.nonEmpty)
      AllPolls.getAll.toList.sortBy(e => e._1.toInt).mkString("\n")
    else
      "Can You see the list of Your polls? I can't too. But they exists."
  }

  def deletePoll(id: Try[List[String]]): String = {
    if (id.isSuccess && RunPolls.get(id.get.head).isSuccess || AllPolls.get(id.get.head).isSuccess) {
      AllPolls.remove(id.get.head)
      RunPolls.remove(id.get.head)
      "Exterminate! Exterminate! Exterminate!"
    }
    else
      "Can't delete Your Poll, cuz there's no such one!"
  }

  def startPoll(id: Try[List[String]]): String = {
    if (id.isSuccess && AllPolls.get(id.get.head).isSuccess) {
      RunPolls.set(id.get.head, AllPolls.get(id.get.head))
      "Your poll was just started, look for feedback!"
    }
    else "Can't start your Poll, cuz there's no such one!"
  }

  def stopPoll(id: Try[List[String]]): String = {
    if (id.isSuccess && RunPolls.get(id.get.head).isSuccess) {
      RunPolls.remove(id.get.head)
      "Your poll was just finished, that was a great poll!"
    }
    else "Cant't stop Your Poll, cuz it's not run!"
  }

  def pollResult(id: Try[List[String]]): String = {
    if (id.isSuccess && AllPolls.get(id.get.head).isSuccess) {
      AllPolls
        .get(id.get.head)
        .map(p => {
          if (p.viewType.eq("continuous") || p.isOver) {
            p.inner.get_result(p.isAnonymous)
          }
          else "You can view the result only when the poll will be over"
        })
        .get
    }
    else "Some trouble was detected, please try again later!"
  }

  def addQuestion(params: Try[List[String]],
                  answers: Try[List[String]]): String = {
    val name = Try(params.get.head)
    val qtype = Try(if (Try(params.get.last).isFailure) "open"
      else parser.parseAll(parser.qtype, params.get.last).get
    )
    if (name.isSuccess && qtype.isSuccess && answers.isSuccess) {
      val n = CurrentPoll.get.inner.set_question(name.get, qtype.get, answers.get)
      "Success: " + n
    }
    else "Can't add the question to your Poll"
  }

  def begin(id: Try[List[String]]): String = {
    if (id.isSuccess && AllPolls.get(id.get.head).isSuccess && CurrentPoll == null) {
      CurrentPoll.set(AllPolls.get(id.get.head))
      "Success"
    }
    else if (id.isSuccess && AllPolls.get(id.get.head).isSuccess && CurrentPoll != null)
      "You've already begun one Poll!"
    else "There is no such Poll"
  }

  def end: String = {
    if (Try(CurrentPoll.setNone()).isSuccess) "Success"
    else "It's impossible to catch a fail in this command! But you've done it!"
  }

  def view: String = {
    if (CurrentPoll != null) ???
    else ???
    //make beauty
  }

//  def deleteQuestion(number : Int) {
//      CurrentPoll.get.questions.remove_question(number)
//  }

  def answer() {}
}
