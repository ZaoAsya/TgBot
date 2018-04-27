import Repository._
import commands.Commands
import poll.Poll
import org.scalatest.FlatSpec


class TestGeneralCommand extends FlatSpec{
  val adm = "Administrator"
  val admID = 299755750
  val usr = "User"
  val userID = 299755678
  val cmdAdm = Commands(admID)
  val cmdUsr = Commands(userID)

  "/list" should "be empty on start" in {
    assertResult("Can You see the list of Your polls? I can't too. But they exists.") {
      cmdAdm.pollList
    }
  }

  "/create_poll" should "run well" in {
    assertResult("Success: 0") {
      cmdAdm.createPoll("name", true, "afterstop", null, null)
    }
    assertResult("Success: 1") {
      cmdAdm.createPoll("name1", false, "afterstop", null, null)
    }
    assertResult("Success: 2") {
      cmdAdm.createPoll("name(12)", true, "continuous", null, null)
    }
    assertResult("Success: 3") {
      cmdAdm.createPoll("name (ty)", true, "continuous", "12:12:12 12:12:12", null)
    }
    assertResult("Success: 4") {
      cmdAdm.createPoll("n a m e", true, "continuous", "12:12:12 12:12:12", "13:13:13 13:13:13")
    }
    assertResult(5) {
      AllPolls.getAll.count(_ => true)
    }
  }

  "/list" should "show settings of all polls" in {
    assertResult("0 => name\n" +
      "is anonymous? true\n" +
      "is it running? false\n" +
      "is over? true\n" +
      "afterstop\n" +
      "starts in: null\n" +
      "ends in: null\n\n" +
      "1 => name1\n" +
      "is anonymous? false\n" +
      "is it running? false\n" +
      "is over? true\n" +
      "afterstop\n" +
      "starts in: null\n" +
      "ends in: null\n\n" +
      "2 => name(12)\n" +
      "is anonymous? true\n" +
      "is it running? false\n" +
      "is over? true\n" +
      "continuous\n" +
      "starts in: null\n" +
      "ends in: null\n\n" +
      "3 => name (ty)\n" +
      "is anonymous? true\n" +
      "is it running? false\n" +
      "is over? true\n" +
      "continuous\n" +
      "starts in: 12:12:12 12:12:12\n" +
      "ends in: null\n\n" +
      "4 => n a m e\n" +
      "is anonymous? true\n" +
      "is it running? false\n" +
      "is over? true\n" +
      "continuous\n" +
      "starts in: 12:12:12 12:12:12\n" +
      "ends in: 13:13:13 13:13:13"){
      cmdAdm.pollList
    }
  }

  "/delete_poll" should "run well" in {
    assert(AllPolls.get("1").isSuccess)
    assertResult("Exterminate! Exterminate! Exterminate!") {
      cmdAdm.deletePoll("1")
    }
    assert(AllPolls.get("1").isFailure)
    assertResult("Can't delete Your Poll, cuz there's no such one!") {
      cmdAdm.deletePoll("1")
    }
  }

  "/begin" should "start working with chosen poll" in {
    assertResult("Let's Rock!"){
      cmdAdm.begin("2")
    }
    assert(CurrentPoll.get(admID) == AllPolls.get("2"))
    assertResult("You've already begun one Poll!"){
      cmdAdm.begin("3")
    }
    assertResult("There is no such Poll!"){
      cmdAdm.begin("12")
    }

    assertResult("Let's Rock!"){
      cmdUsr.begin("2")
    }
    assert(CurrentPoll.get(userID) == AllPolls.get("2"))
    assertResult("You've already begun one Poll!"){
      cmdUsr.begin("3")
    }
    assertResult("There is no such Poll!"){
      cmdUsr.begin("12")
    }
  }

  "/add_question" should "add question to current poll (Admin)" in {}

  "/answer" should "save the answer on question by id" in {}

  "/delete_question" should "delete question by id in current poll (Admin)" in {}

  "/end" should "stop working with current poll" in {
    assertResult("Now, you're free!"){
      cmdAdm.end
    }
    assertResult("You have no begun Poll!"){
      cmdAdm.end
    }

    assertResult("Now, you're free!"){
      cmdUsr.end
    }
    assertResult("You have no begun Poll!"){
      cmdUsr.end
    }
  }

  "/start_poll" should "run well" in { //пока нет условия про начальное время
    assert(AllPolls.getAllRun.isEmpty)
    assertResult("Your poll was just started, look for feedback!") {
      cmdAdm.startPoll("0")
    }
    assert(AllPolls.getAllRun.nonEmpty)
    assertResult("Can't start your Poll, cuz there's no such one!") {
      cmdAdm.startPoll("1")
    }
  }

  "/stop_poll" should "run well" in { //нет условия про конечное время
    assert(AllPolls.getRun("0").isSuccess)
    assertResult("Your poll was just finished, that was a great poll!") {
      cmdAdm.stopPoll("0")
    }
    assert(AllPolls.getRun("0").isFailure)
    assertResult("Cant't stop Your Poll, cuz it's not run!") {
      cmdAdm.stopPoll("0")
    }
  }

  "/view" should "show current poll with its questions and answers" in {
    val poll = Poll("NNNName", true, "afterstop", null, null, false, List("quest1", "quest2"),
      Map((1, List("ans11", "ans12")), (2, List("ans21", "ans22"))))
    CurrentPoll.set(userID, poll)
    print(cmdUsr.view)
  }

  "/result" should "show the results of chosen poll" in {}
}