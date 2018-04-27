package Repository

import poll.Poll

import scala.util.{Failure, Success, Try}

object AllPolls {
  private var P = Map[String, Poll]()

  def get(id: String): Try[Poll] =
    P.get(id).map(Success(_)).getOrElse(Failure(new Exception))

  def set(id: String, poll: Poll): Unit = P = P updated (id, poll)

  def remove(id: String): Unit = P = P - id

  def getAll: Map[String, Poll] = P

  def get_id(): String = {
    (if (P.toList.nonEmpty) P.toList.maxBy(_._1)._1.toInt + 1 else 0).toString
  }

  def getRun(id: String): Try[Poll] =
    P.get(id).filter(_.isRun).map(Success(_)).getOrElse(Failure(new Exception()))

  def contains(poll : Poll) : Boolean = {
    P.values.toList.contains(poll)
  }

  def containsRun(poll : Poll) : Boolean = {
    getAllRun.values.toList.contains(poll)
  }

  def setRun(id: String, poll: Poll): Option[Unit] =
    P.get(id).filter(!_.isRun).map(p => P = P updated(id, p.copy(isRun = true)))

  def removeRun(id: String): Option[Unit] =
    P.get(id).filter(_.isRun).map(p => P = P updated(id, p.copy(isRun = false)))

  def getAllRun: Map[String, Poll] = P.filter(e => e._2.isRun)
}
