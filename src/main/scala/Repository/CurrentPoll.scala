package Repository

import poll.Poll

import scala.util.{Failure, Success, Try}
import com.softwaremill.quicklens._



object CurrentPoll {
  private var P : Map[Int, Poll] = Map()
  //                userID  Poll

  def get(id : Int): Try[Poll] = P.get(id).map(Success(_)).getOrElse(Failure(new Exception))

  def set(id : Int, poll : Poll) : Unit = P = P updated (id, poll)

  def setNone(id : Int) : Unit = P = P - id
}


//package Repository
//
//import poll.Poll
//
//object CurrentPoll {
//  private var P : Option[Poll] = Option.empty
//
//  def get : Option[Poll] = P
//
//  def set(poll : Poll) : Unit = P = Option(poll)
//
//  def setNone() : Unit = P = Option.empty
//}