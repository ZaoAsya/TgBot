package poll

class Inner {
  private var inner : Map[(String, Int, Int), List[Int]] = Map[(String, Int, Int), List[Int]]()
  //                       pollId  numQ numA    people

  def get(pollId: String, numQ: Int, numA: Int): List[Int]= {
    inner.getOrElse((pollId, numQ, numA), List())
  }

  def set(pollId: String, numQ: Int, numA: Int, answer: Int): Unit ={
    val list : List[Int] = inner.getOrElse((pollId, numQ, numA), List()) :+ answer
    inner = inner updated ((pollId, numQ, numA), list)
  }
}