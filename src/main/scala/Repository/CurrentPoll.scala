package Repository

import poll.Poll

import scala.util.Try

object CurrentPoll {
  private var P : Poll = null

  def get : Poll = P

  def set(poll : Poll) : Unit = P = poll

  def set(poll : Try[Poll]): Try[Unit] = {
    poll.map(p => P = p)
  }

  def setNone() : Unit = P = null
}

//
//class CurrentPoll(poll: Poll){
//  val cPoll : Poll = poll
//}