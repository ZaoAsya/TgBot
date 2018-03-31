import Repository._
import commands.General._
import interaction.Reader._
import org.scalatest._


class TestParsing extends FlatSpec {

  "/list" should "be empty on start" in {
    assertResult("Can You see the list of Your polls? I can't too. But they exists.") {
      createCommands("/list").get.execute()
    }
  }

  "/create_poll" should "be parsed right" in {
    assert(createCommands("/create_poll (fghjkhk((akjdj)))").isSuccess)
    assertResult("Success: 0") {
      createCommands("/create_poll (name0)").get.execute()
    }
    assertResult("Success: 1") {
      createCommands("/create_poll (name ((1))) (yes)").get.execute()
    }
    assertResult("Success: 2") {
      createCommands("/create_poll (n a m e 2) (no) (afterstop)").get.execute()
    }
    assertResult("Success: 3") {
      createCommands("/create_poll (na((me)) 3) (yes) (continuous) (23:23:23 23:23:23)")
        .get.execute()
    }
    assertResult("Success: 4") {
      createCommands("/create_poll (name4) (yes) (continuous) (23:23:23 23:23:23)" +
        " (25:25:25 25:25:25)").get.execute()
    }
    print(pollList())
  }

  "CreateCommand" should "doesn't know strange input" in {
    assertResult("Unrecognised command! Say what!?")(createCommands("/creat_pol (qwerty)").get.execute())
  }

  "/delete_poll" should "be parsed right" in {
    assertResult("Exterminate! Exterminate! Exterminate!") {
      createCommands("/delete_poll (0)").get.execute()
    }
    assert(AllPolls.get("0").isFailure)
    assertResult("Can't delete Your Poll, cuz there's no such one!") {
      createCommands("/delete_poll ").get.execute()
    }
    assertResult("Can't delete Your Poll, cuz there's no such one!") {
      createCommands("/delete_poll ()").get.execute()
    }
    assertResult("Can't delete Your Poll, cuz there's no such one!") {
      createCommands("/delete_poll (5)").get.execute()
    }
    assertResult("Can't delete Your Poll, cuz there's no such one!") {
      createCommands("/delete_poll (d)").get.execute()
    }
  }

  "/start_poll" should "be parsed right" in {
    assertResult("Your poll was just started, look for feedback!") {
      createCommands("/start_poll (1)").get.execute()
    }
    assert(RunPolls.get("1").isSuccess)
    assertResult("Can't start your Poll, cuz there's no such one!") {
      createCommands("/start_poll ()").get.execute()
    }
    assertResult("Can't start your Poll, cuz there's no such one!") {
      createCommands("/start_poll (d)").get.execute()
    }
    assertResult("Can't start your Poll, cuz there's no such one!") {
      createCommands("/start_poll (12)").get.execute()
    }
  }

  "/stop_poll" should "be parsed right" in {
    assertResult("Your poll was just finished, that was a great poll!") {
      createCommands("/stop_poll (1)").get.execute()
    }
    assert(RunPolls.get("1").isFailure)
    assertResult("Cant't stop Your Poll, cuz it's not run!") {
      createCommands("/stop_poll (12)").get.execute()
    }
    assertResult("Cant't stop Your Poll, cuz it's not run!") {
      createCommands("/stop_poll (f)").get.execute()
    }
  }
}