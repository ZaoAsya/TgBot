package Repository

import poll.Poll

import scala.util.Try

object CurrentPoll {
  private var P : Poll = _

  def get : Poll = P

  def set(poll : Poll) : Unit = P = poll

  def set(poll : Try[Poll]): Try[Unit] = poll.map(p => P = p)

  def setNone() : Unit = P = null
}
