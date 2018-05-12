import parsers.CommandParser._
import org.scalatest._


class TestParsing extends FlatSpec {
  val adm = "Administrator"
  val admID = 299755750
  val usr = "User"
  val userID = 299755678

  "/list" should "be parsed right" in {
    assert(parse("/list", adm, admID).isSuccess)
    assert(parse("/list", usr, userID).isSuccess)
//    assertResult("Can You see the list of Your polls? I can't too. But they exists.") {
//      parse("/list", adm, admID).get
//    }
//    assertResult("Can You see the list of Your polls? I can't too. But they exists.") {
//      parse("/list", usr, userID).get
//    }
  }

  "/create_poll" should "be parsed right (Admin)" in {
    assert(parse("/create_poll (name0)", adm, admID).isSuccess)
    assert(parse("/create_poll (name ((1))) (yes)", adm, admID).isSuccess)
    assert(parse("/create_poll (n a m e 2) (no) (afterstop)", adm, admID).isSuccess)
    assert(parse("/create_poll (na((me)) 3) (yes) (continuous) (23:23:23 23:23:23)", adm, admID).isSuccess)
    assert(parse("/create_poll (name4) (yes) (continuous) (23:23:23 23:23:23) (25:25:25 25:25:25)", adm, admID).isSuccess)
    assert(parse("/create_poll (name ((1))) (ys)", adm, admID).isFailure)
    assert(parse("/create_poll (n a m e 2) (no) (aftop)", adm, admID).isFailure)
    assert(parse("/create_poll (na((me)) 3) (yes) (continuous) (23:23:23 23:2:23)", adm, admID).isFailure)
//    assertResult("Success: 0") {
//      parse("/create_poll (name0)", adm, admID).get
//    }
//    assertResult("Success: 1") {
//      parse("/create_poll (name ((1))) (yes)", adm, admID).get
//    }
//    assertResult("Success: 2") {
//      parse("/create_poll (n a m e 2) (no) (afterstop)", adm, admID).get
//    }
//    assertResult("Success: 3") {
//      parse("/create_poll (na((me)) 3) (yes) (continuous) (23:23:23 23:23:23)", adm, admID)
//        .get
//    }
//    assertResult("Success: 4") {
//      parse("/create_poll (name4) (yes) (continuous) (23:23:23 23:23:23)" +
//        " (25:25:25 25:25:25)", adm, admID).get
//    }
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
//    assertResult("Exterminate! Exterminate! Exterminate!") {
//      parse("/delete_poll (0)", adm, admID).get
//    }
//    assert(AllPolls.get(0).isFailure)
    assert(parse("/delete_poll (0)", adm, admID).isSuccess)
    assert(parse("/delete_poll ", adm, admID).isFailure)
    assert(parse("/delete_poll ()", adm, admID).isFailure)
    assert(parse("/delete_poll (d)", adm, admID).isFailure)
  }

  "/begin" should "be parsed right" in {
    //    assertResult("There is no such Poll!") {
    //      parse("/begin (12)", adm, admID).get
//    }
//    assertResult("Let's Rock!") {
//      parse("/begin (3)", adm, admID).get
//    }
//    assert(CurrentPoll.get(admID) == AllPolls.get(3))
    //    assertResult("You've already begun one Poll!") {
    //      parse("/begin (4)", adm, admID).get
    //    }
//    assertResult("Let's Rock!") {
    //      parse("/begin (4)", usr, userID).get
    //    }
    //    assert(CurrentPoll.get(userID) == AllPolls.get(4))
    //    assertResult("You've already begun one Poll!") {
    //      parse("/begin (4)", usr, userID).get
    //    }
    assert(parse("/begin (a)", adm, admID).isFailure)
    assert(parse("/begin (3)", adm, admID).isSuccess)
    assert(parse("/begin (4)", usr, userID).isSuccess)
  }

  "/add_question" should "add question to current poll (Admin)" in {
    assert(parse("/add_question (new question?) (open)" +
                 """
ans1
ans2
asn3""", adm, admID).isFailure)
    assert(parse("/add_question (question?) (multi)", adm, admID).isFailure)
    assert(parse("/add_question (question2?) (choice)", adm, admID).isFailure)
    assert(parse("/add_question (new question?) (wrong) ans1\nans2", adm, admID).isFailure)
    assert(parse("/add_question", adm, admID).isFailure)

    assert(parse("/add_question (What's up?) (open)", adm, admID).isSuccess)
    assert(parse("/add_question (choice question?) (choice)" +
      """
ans1
ans2
ans3""", adm, admID).isSuccess)
    assert(parse("/add_question (multi question?) (multi)" +
      """
ans1
ans2
ans3""", adm, admID).isSuccess)
//    assertResult("Success: 1"){
//      parse("/add_question (choice question?) (choice)" +
//        """
//ans1
//ans2
//ans3""", adm, admID).get
//    }
//    assertResult("Success: 2"){
//      parse("/add_question (multi question?) (multi)" +
//            """
//ans1
//ans2
//ans3""", adm, admID).get
//    }
  }

  "/delete_question" should "be parsed right (Admin)" in {
    assert(parse("/delete_question (a)", adm, admID).isFailure)
    assert(parse("/delete_question", adm, admID).isFailure)
    assert(parse("/delete_question (0)", adm, admID).isSuccess)
    //    assert(CurrentPoll.get(admID).get.questions.length == 3)
    //    assertResult("Success"){
//      parse("/delete_question (0)", adm, admID).get
//    }
//    assert(CurrentPoll.get(admID).get.questions.length == 2)
//    assertResult("Can't delete this Question, set the right Poll or question!"){
//      parse("/delete_question (4)", adm, admID).get
//    }
  }

  "/end" should "be parsed right" in {
    assert(parse("/end", adm, admID).isSuccess)
    assert(parse("/end", usr, userID).isSuccess)
//    assertResult("Now, you're free!") {
//      parse("/end", adm, admID).get
//    }
//    assert(CurrentPoll.get(admID).isFailure)
//    assertResult("You have no begun Poll!") {
//      parse("/end", adm, admID).get
//    }

//    assertResult("Now, you're free!") {
//      parse("/end", usr, userID).get
//    }
//    assert(CurrentPoll.get(userID).isFailure)
//    assertResult("You have no begun Poll!") {
//      parse("/end", usr, userID).get
//    }
  }

  "/start_poll" should "be parsed right (Admin)" in {
    assert(parse("/start_poll (1)", adm, admID).isSuccess)
    assert(parse("/start_poll ()", adm, admID).isFailure)
    assert(parse("/start_poll (d)", adm, admID).isFailure)
    assert(parse("/start_poll", adm, admID).isFailure)
    //    assertResult("Your poll was just started, look for feedback!") {
    //      parse("/start_poll (1)", adm, admID).get
    //    }
    //    assert(AllPolls.getRun(1).isSuccess)
//    assertResult("Can't start your Poll, cuz there's no such one!") {
//      parse("/start_poll (12)", adm, admID).get
//    }
  }

  "/answer" should "be parsed right" in {

    assert(parse("/answer", adm, admID).isFailure)
    assert(parse("/answer (j)", adm, admID).isFailure)

    assert(parse("/answer", usr, userID).isFailure)
    assert(parse("/answer (jk)", usr, userID).isFailure)

    assert(parse("/answer (0) (1)", adm, admID).isSuccess)
    assert(parse("/answer (0) (1 2)", adm, admID).isSuccess)
    assert(parse("/answer (0) (answer)", adm, admID).isSuccess)

    assert(parse("/answer (0) (1)", usr, userID).isSuccess)
    assert(parse("/answer (0) (1 2)", usr, userID).isSuccess)
    assert(parse("/answer (0) (answer)", usr, userID).isSuccess)
  }

  "/stop_poll" should "be parsed right (Admin)" in {
    assert(parse("/stop_poll (1)", adm, admID).isSuccess)
    assert(parse("/stop_poll ()", adm, admID).isFailure)
    assert(parse("/stop_poll (d)", adm, admID).isFailure)
    assert(parse("/stop_poll", adm, admID).isFailure)
//    assertResult("Your poll was just finished, that was a great poll!") {
//      parse("/stop_poll (1)", adm, admID).get
//    }
//    assert(AllPolls.getRun(1).isFailure)
//    assertResult("Cant't stop Your Poll, cuz it's not run!") {
//      parse("/stop_poll (12)", adm, admID).get
//    }
//    assert(parse("/stop_poll (f)", adm, admID).isFailure)
  }

  "/view" should "be parsed right" in {
    assert(parse("/view", adm, admID).isSuccess)
    assert(parse("/view", usr, userID).isSuccess)
  }

  "/result" should "be parsed right" in {
    assert(parse("/result (1)", adm, admID).isSuccess)
    assert(parse("/result (h)", adm, admID).isFailure)
    assert(parse("/result (sd)", usr, userID).isFailure)
    assert(parse("/result (1h)", adm, admID).isFailure)
    assert(parse("/result", adm, admID).isFailure)
    assert(parse("/result ()", adm, admID).isFailure)
  }
}