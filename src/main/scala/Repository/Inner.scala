package Repository

object Inner {
  private var inner : Map[(Int, Int, Int), Vector[Int]] = Map[(Int, Int, Int), Vector[Int]]()
  //                       pollId  numQ numA    people

  def get(pollId: Int, numQ: Int, numA: Int): Vector[Int]= {
    inner.getOrElse((pollId, numQ, numA), Vector())
  }

  def set(pollId: Int, numQ: Int, numA: Int, answer: Int): Unit ={
    val list : Vector[Int] = inner.getOrElse((pollId, numQ, numA), Vector()) :+ answer
    inner = inner updated ((pollId, numQ, numA), list)
  }
}