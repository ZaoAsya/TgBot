package commands

import Repository._
import poll.Poll

import scala.util.Try

case class Commands(userID : Int) {
//  private def getMonth(month: String): String = {
//    month match {
//      case "Jan" => "01"
//      case "Feb" => "02"
//      case "Mar" => "03"
//      case "Apr" => "04"
//      case "May" => "05"
//      case "Jun" => "06"
//      case "Jul" => "07"
//      case "Aug" => "08"
//      case "Sep" => "09"
//      case "Oct" => "10"
//      case "Nov" => "11"
//      case "Dec" => "12"
//    }
//  }

//  private def getDateTime(input: String): String = {
//    val date = input.split(' ')
//    date(3) + ' ' + date(5).drop(2) + ':' + getMonth(date(1)) + ':' + date(2)
//  }
  def createPoll(title: String,
                 anonymous: Boolean,
                 viewType: String,
                 startTime: String,
                 stopTime: String): String = {
    val poll = Poll(title, anonymous, viewType, startTime, stopTime)
    val id = AllPolls.get_id()
    AllPolls.set(id, poll)
    "Success: " + id
  }

  def pollList: String = {
    if (AllPolls.getAll.nonEmpty)
      AllPolls.getAll.toList.sortBy(e => e._1.toInt).map(e =>
        e._1 + " => " + e._2.name
          + "\nis anonymous? " + e._2.isAnonymous
          + "\nis it running? " + e._2.isRun
          + "\nis over? " + !e._2.isRun
          + "\n" + e._2.viewType
          + "\nstarts in: " + e._2.startTime
          + "\nends in: " + e._2.stopTime).mkString("\n\n")
    else
      "Can You see the list of Your polls? I can't too. But they exists."
  }

  def deletePoll(id: String): String = {
    if (AllPolls.getRun(id).isSuccess || AllPolls.get(id).isSuccess) {
      AllPolls.remove(id)
      AllPolls.removeRun(id)
      "Exterminate! Exterminate! Exterminate!"
    }
    else
      "Can't delete Your Poll, cuz there's no such one!"
  }

  def startPoll(id: String): String = {
    if (AllPolls.get(id).isSuccess) {
      AllPolls.setRun(id, AllPolls.get(id).get)
      "Your poll was just started, look for feedback!"
    }
    else "Can't start your Poll, cuz there's no such one!"
  }

  def stopPoll(id: String): String = {
    if (AllPolls.getRun(id).isSuccess) {
      AllPolls.removeRun(id)
      "Your poll was just finished, that was a great poll!"
    }
    else "Cant't stop Your Poll, cuz it's not run!"
  }

  def pollResult(id: String): String = ???
//    if (AllPolls.get(id).isSuccess) {
//      AllPolls
//        .get(id)
//        .map(p => {
//          if (p.viewType.eq("continuous") || !p.isRun) {
//            p.inner.get_result(p.isAnonymous)
//          }
//          else "You can view the result only when the poll will be over"
//        })
//        .get
//    }
//    else "Some trouble was detected, please try again later!"
//  }

  def addQuestion(qustion: String, qType: String, answers: List[String]): String = {???}
//    val name = Try(params.head)
//
//    val currentPoll = CurrentPoll.get(userID)
//
//    val qtype : Try[String] = params.lift(1).fold(Try("open")) { questionType =>
//      Try(ParamsParser.parseAll(ParamsParser.qtype, questionType).get)
//    }
//
//    (for {
//      n <- name
//      cp <- currentPoll
//      q <- qtype
//      if !AllPolls.containsRun(cp)
//    }
//      yield  "Success: " + cp.inner.set_question(n, q, answers)) match {
//      case Success(s) => s
//      case Failure(_) => "Can't add the question to your Poll"
//    }
//  }

  def begin(id: String): String = {
    if (AllPolls.get(id).isSuccess && CurrentPoll.get(userID).isFailure) {
      CurrentPoll.set(userID, AllPolls.get(id).get)
      "Let's Rock!"
    }
    else if (AllPolls.get(id).isSuccess && CurrentPoll.get(userID).isSuccess)
      "You've already begun one Poll!"
    else "There is no such Poll!"
  }

  def end: String = {
    if (CurrentPoll.get(userID).isSuccess && Try(CurrentPoll.setNone(userID)).isSuccess) "Now, you're free!"
    else "You have no begun Poll!"
  }

  def view: String = {
    if (CurrentPoll.get(userID).isSuccess) {
      val cp = CurrentPoll.get(userID).get
      cp.name + (for {
        q <- cp.questions
        a <- cp.answers.getOrElse(cp.questions.indexOf(q), Vector())
      } yield q + " => " + a).mkString("\n")
    }
    else "There's no current Poll!"
  }

  def deleteQuestion(number : String): String = {???}
//    val currentPoll = CurrentPoll.get(userID)
//    if (currentPoll.isSuccess && Try(currentPoll.get.inner.remove_question(number.head.toInt)).isSuccess)
//      "I ate this for you!"
//    else "Can't delete this Question, set the right Poll or question!"
//  }

  def answerDaemon(params : String) : String = {???}
//    val currentPoll : Try[Poll] = CurrentPoll.get(userID)
//    val idc = Try(params.head.toInt)
//    val answers = Try(params(1))
//    if (currentPoll.isSuccess && AllPolls.containsRun(CurrentPoll.get(userID).get)
//      && idc.isSuccess && currentPoll.get.inner.get_question(idc.get).isSuccess && answers.isSuccess) {
//      val parserdParams = Try(currentPoll.get.inner.get_question_type(idc.get).get match {
//        case "open" =>
//          answers.get
//        case "choice" =>
//          val ans = Try(answers.get.toInt)
//          if (ans.isSuccess && currentPoll.get.inner.get_answer(idc.get, ans.get).isSuccess)
//            ans.get.toString
//          else Failure(new Exception)
//        case "multi" =>
//          val parsedAnswers = Try(ParamsParser.parseAll(ParamsParser.multi, answers.get))
//          if (parsedAnswers.isSuccess) {
//            parsedAnswers.get.get.toSet.map(e => e.toInt)
//          }
//          else Failure(new Exception)
//      })
//      if (parserdParams.isSuccess) answer(parserdParams.get.toString) else "WTF&?!"
//    }
//    else "WTF&!?"
//  }

  def answer(answer : String): String = {???}
//    "Success: " + CurrentPoll.get(userID).get.inner.set_answer()
//  }
}
