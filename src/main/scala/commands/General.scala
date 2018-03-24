package commands

import java.util.Date

import Repository._
import interaction.{ParamsParser, Writer}
import poll.Poll

import scala.util.Try

object General {
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
    val parser = new ParamsParser()

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
    id
  }

  def pollList(): String = {
    if (Try(AllPolls.getAll).isSuccess && AllPolls.getAll.nonEmpty)
      AllPolls.getAll.toList.sortBy(e => e._1.toInt).mkString("\n")
    else
      "Can You see the list of Your polls? I can't too. But they exists."
  }

  def deletePoll(id: Try[List[String]]): String = {
    if (Try {
          val n = id.get.head
          AllPolls.remove(n)
          RunPolls.remove(n)
        }.isSuccess)
      "Exterminate! Exterminate! Exterminate!"
    else
      "Can't delete Your Poll, maybe id is not set up!"
  }

  def startPoll(id: Try[List[String]]): String = {
    if (id.isSuccess) {
      val n = id.get.head
      if (RunPolls.set(n, AllPolls.get(n)).isSuccess)
        "Your poll was just started, look for feedback!"
      else "Can't start your Poll, cuz there's no such Poll! Please try again later!"
    }
    else "Can't start your Poll! Choose id!"
  }

  def stopPoll(id: Try[List[String]]): String = {
    if(id.isSuccess)
      if (RunPolls.get(id.get.head).isSuccess) {
        RunPolls.remove(id.get.head)
        "Your poll was just finished, that was a great poll!"
      }
      else "Cant't stop Your Poll, it isn't run!"
    else "Cant't stop Your Poll, choose id!"
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
    if (!params.isSuccess) "Wrong parameters"
    else if (!answers.isSuccess) "Wrong format of answers"
    else {
      val name = Try(params.get.head)
      val qtype = Try(params.get.last)
      CurrentPoll.get.inner.set_question(name, qtype, answers.get)
      "Success"
    }
  }

  def begin(id: Try[List[String]]): String = {
    if (id.isSuccess) {
      CurrentPoll.set(AllPolls.get(id.get.head))
      "Success"
    } else {
      "There is no such Poll"
    }
  }

  def end: String = {
    try {
      CurrentPoll.setNone()
      "Success"
    } catch {
      case _: Exception =>
        "It's impossible to catch a fail in this command! But you've done it!"
    }
  }

  def view: String = {
    Writer.write(CurrentPoll.get)
    // make a beauty
    ""
  }

//  def deleteQuestion(number : Int) {
//      CurrentPoll.get.questions.remove_question(number)
//  }

  def answer() {}
}
