package Repository

import poll.Poll

import scala.collection.mutable
import scala.util.Try


object AllPolls {
  private var P = Map[String, Poll]()

  private var  ID = 0

  def get(id : String) : Try[Poll] = Try{P(id)}

  def set(id : String, poll : Poll) : Unit = P = P updated(id, poll)

  def remove(id : String) : Unit = P = P - id

  def getAll : Map[String, Poll] = P

  def get_id(): String ={
    val n = this.ID
    this.ID += 1
    n.toString
  }

}
