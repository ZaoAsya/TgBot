package Repository

import poll.Poll

import scala.collection.mutable
import scala.util.Try


object RunPolls {
  private var P = Map[String, Poll]()

  def get(id : String) : Try[Poll] = Try{P(id)}

  def set(id : String, poll : Try[Poll]) : Try[Unit] = Try{P = P updated (id, poll.get)}

  def set(id : String, poll : Poll) : Unit = P = P updated (id, poll)

  def remove(id : String) : Unit = P = P - id

  def getAll : Map[String, Poll] = P
}
