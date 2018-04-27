import Repository._
import parsers.CommandParser._
import org.scalatest._


class TestParsing extends FlatSpec {
  val adm = "Administrator"
  val admID = 299755750
  val usr = "User"
  val userID = 299755678

  "/list" should "be empty on start" in {
    assertResult("Can You see the list of Your polls? I can't too. But they exists.") {
      parse("/list", adm, admID).get
    }
    assertResult("Can You see the list of Your polls? I can't too. But they exists.") {
      parse("/list", usr, userID).get
    }
  }

  "/create_poll" should "finish right (Admin)" in {
    assertResult("Success: 0") {
      parse("/create_poll (name0)", adm, admID).get
    }
    assertResult("Success: 1") {
      parse("/create_poll (name ((1))) (yes)", adm, admID).get
    }
    assertResult("Success: 2") {
      parse("/create_poll (n a m e 2) (no) (afterstop)", adm, admID).get
    }
    assertResult("Success: 3") {
      parse("/create_poll (na((me)) 3) (yes) (continuous) (23:23:23 23:23:23)", adm, admID)
        .get
    }
    assertResult("Success: 4") {
      parse("/create_poll (name4) (yes) (continuous) (23:23:23 23:23:23)" +
        " (25:25:25 25:25:25)", adm, admID).get
    }
  }

  "Common User" should "have no privileges for some commands" in {
    assertResult("You don't have such level of privileges"){
      parse("/create_poll (name0)", usr, userID).get
    }
    assertResult("You don't have such level of privileges"){
      parse("/delete_poll (1)", usr, userID).get
    }
    assertResult("You don't have such level of privileges"){
      parse("/start_poll (0)", usr, userID).get
    }
    assertResult("You don't have such level of privileges"){
      parse("/stop_poll (0)", usr, userID).get
    }
    assertResult("You don't have such level of privileges"){
      parse("/add_question (asdfhj) (fghj)", usr, userID).get
    }
    assertResult("You don't have such level of privileges"){
      parse("/delete_question (0)", usr, userID).get
    }
  }

  "CommandParser" should "doesn't know strange input" in {
    assertResult("Unrecognised command! Say what!?") {
      parse("/creat_pol (qwerty)", adm, admID).get
    }
  }

  "/delete_poll" should "be parsed right (Admin)" in {
    assertResult("Exterminate! Exterminate! Exterminate!") {
      parse("/delete_poll (0)", adm, admID).get
    }
    assert(AllPolls.get(0).isFailure)
    assert(parse("/delete_poll ", adm, admID).isFailure)
    assert(parse("/delete_poll ()", adm, admID).isFailure)
    assertResult("Can't delete Your Poll, cuz there's no such one!") {
      parse("/delete_poll (12)", adm, admID).get
    }
    assert(parse("/delete_poll (d)", adm, admID).isFailure)
  }

  "/begin" should "start working with chosen poll" in {
    assert(parse("/begin (a)", adm, admID).isFailure)
    assertResult("There is no such Poll!") {
      parse("/begin (12)", adm, admID).get
    }
    assertResult("Let's Rock!") {
      parse("/begin (3)", adm, admID).get
    }
    assert(CurrentPoll.get(admID) == AllPolls.get(3))
    assertResult("You've already begun one Poll!") {
      parse("/begin (4)", adm, admID).get
    }

    assertResult("Let's Rock!") {
      parse("/begin (4)", usr, userID).get
    }
    assert(CurrentPoll.get(userID) == AllPolls.get(4))
    assertResult("You've already begun one Poll!") {
      parse("/begin (4)", usr, userID).get
    }
  }

  "/add_question" should "add question to current poll (Admin)" in {
    //    assertResult("Success: 0"){
    //      parse("/add_question (new question?) (open)\nans1\nans2\nans3", adm, admID)
    //    }
    assert(parse("/add_question (new question?) (open) ans1\nans2\nans3", adm, admID).isFailure)
    assert(parse("/add_question", adm, admID).isFailure)
    //    assertResult("Success: 1"){
    //      parse("/add_question (new question?) (open)\nans1\nans2\nans3", adm, admID)
    //    }
  }

  "/delete_question" should "delete question by id in current poll (Admin)" in {
    assert(parse("/delete_question (a)", adm, admID).isFailure)
    assert(parse("/delete_question", adm, admID).isFailure)
  }

  "/answer" should "save the answer on question by id" in {
    assert(parse("/answer", adm, admID).isFailure)
    assert(parse("/answer (j)", adm, admID).isFailure)

    assert(parse("/answer", usr, userID).isFailure)
    assert(parse("/answer (jk)", usr, userID).isFailure)

  }

  "/end" should "stop working with current poll" in {
    assertResult("Now, you're free!") {
      parse("/end", adm, admID).get
    }
    assert(CurrentPoll.get(admID).isFailure)
    assertResult("You have no begun Poll!") {
      parse("/end", adm, admID).get
    }

    assertResult("Now, you're free!") {
      parse("/end", usr, userID).get
    }
    assert(CurrentPoll.get(userID).isFailure)
    assertResult("You have no begun Poll!") {
      parse("/end", usr, userID).get
    }
  }

  "/start_poll" should "be parsed right (Admin)" in {
    assertResult("Your poll was just started, look for feedback!") {
      parse("/start_poll (1)", adm, admID).get
    }
    assert(AllPolls.getRun(1).isSuccess)
    assert(parse("/start_poll ()", adm, admID).isFailure)
    assert(parse("/start_poll (d)", adm, admID).isFailure)
    assertResult("Can't start your Poll, cuz there's no such one!") {
      parse("/start_poll (12)", adm, admID).get
    }
  }

  "/stop_poll" should "be parsed right (Admin)" in {
    assertResult("Your poll was just finished, that was a great poll!") {
      parse("/stop_poll (1)", adm, admID).get
    }
    assert(AllPolls.getRun(1).isFailure)
    assertResult("Cant't stop Your Poll, cuz it's not run!") {
      parse("/stop_poll (12)", adm, admID).get
    }
    assert(parse("/stop_poll (f)", adm, admID).isFailure)
  }

  "/view" should "show current poll with its questions and answers" in {}

  "/result" should "show the results of chosen poll" in {}
}