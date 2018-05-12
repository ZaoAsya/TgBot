package Repository

object Inner {
  private var inner : Map[(Int, Int, Int), Vector[(Int, String)]] = Map[(Int, Int, Int), Vector[(Int, String)]]()
  //                      pollId  numQ numA       people answer

  def get(pollId: Int, numQ: Int, numA: Int): Vector[(Int, String)]= {
    inner.getOrElse((pollId, numQ, numA), Vector())
  }

  def set(pollId: Int, numQ: Int, numA: Int, user: Int, answer: String): Unit ={
    val list : Vector[(Int, String)] = inner.getOrElse((pollId, numQ, numA), Vector()) :+ (user, answer)
    inner = inner updated ((pollId, numQ, numA), list)
  }
}