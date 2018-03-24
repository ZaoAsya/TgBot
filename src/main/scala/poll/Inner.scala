package poll

import scala.util.Try

class Inner {
  private var inner = Map[Int, Map[String, Map[Int, Map[String, List[String]]]]]()
  //numQ   question     numA      answer    people

  def get_question(numQ: Int): Option[String] = inner.get(numQ).map(m => m.keys.toList.head)

  def get_questions(): List[String] = inner.values.map(m => m.keys.toList.head).toList

  def get_answer(numQ: Int, numA: Int): Option[String] =
    inner.get(numQ).flatMap(m => m.values.toList.head.get(numA).map(e => e.keys.toList.head))

  def get_answers(numQ: Int): Option[List[String]] =
    inner.get(numQ).map(m => m.values.toList.head.values.flatMap(e => e.keys.toList).toList)

  def get_votes(numQ: Int, numA: Int): Option[List[String]] =
    inner.get(numQ).flatMap(m => m.values.toList.head.get(numA).map(e => e.values.toList.head))

  def set_question(name: Try[String], qtype: Try[String], answers: List[String]) {}

  def set_answer() {}

  def set_votes() {}

  //  def get_result(isAnonymous : Boolean): String = {
  //    val res_str : String = {
  //      for {
  //        i <- 0 until get_questions().length
  //        j <- 0 until get_answers(i).get.length
  //        if isAnonymous
  //          List((i + 1).toString, get_question(i).get, (j + 1).toString,
  //            get_answer(i, j).get,get_votes(i, j).get.toString, "\n")
  //          .mkString(" ")
  //        if !isAnonymous
  //          List((i + 1).toString, get_question(i).get, (j + 1).toString,
  //            get_answer(i, j).get, get_votes(i, j).get.length.toString, "\n")
  //          .mkString(" ")
  //      } yield ()
  ////      for (i <- 0 until  get_questions().length){
  ////        get_answers(i).get
  ////      }
  //    }
  //  }
  def get_result(isAnonymous: Boolean): String = {
    val res_str: List[String] = get_questions().indices.map(i => get_answers(i).get.indices.foreach(j => {
      if (isAnonymous)
        List((i + 1).toString, get_question(i).get, (j + 1).toString,
          get_answer(i, j).get, get_votes(i, j).get.toString, "\n")
          .mkString(" ")
      else
        List((i + 1).toString, get_question(i).get, (j + 1).toString,
          get_answer(i, j).get, get_votes(i, j).get.length.toString, "\n")
          .mkString(" ")
    }).toString).toList
    res_str.mkString("\n")



    //  var res_str = ""
    //  for (i <- get_questions().indices){
    //      for(j <- get_answers(i).get.indices){
    //        if (isAnonymous)
    //          res_str = res_str + List((i + 1).toString, get_question(i).get, (j + 1).toString,
    //            get_answer(i, j).get,get_votes(i, j).get.toString, "\n")
    //          .mkString(" ")
    //        else
    //          res_str = res_str + List((i + 1).toString, get_question(i).get, (j + 1).toString,
    //            get_answer(i, j).get, get_votes(i, j).get.length.toString, "\n")
    //          .mkString(" ")
    //      }
    //    }
    //    res_str
    //  }
  }
}