package timers
import com.github.nscala_time.time.Imports._

import Repository.AllPolls

object PollTimer extends Runnable{
  override def run(): Unit = {
    while (true) {
      AllPolls.getAll.filter(_._2.stopTime.get > DateTime.now()).map(t => AllPolls.removeRun(t._2.id))
      AllPolls.getAll.filter(_._2.startTime.get < DateTime.now()).map(t => AllPolls.setRun(t._2.id))
      Thread.sleep(1000)
    }
  }
}
