package poll

class Poll(title: String,
           anonymous: Boolean,
           view: String,
           start: String,
           finish: String) {
  val name: String = title
  val isAnonymous: Boolean = anonymous
  val viewType: String = view
  val startTime: String = start
  val stopTime: String = finish
  val isOver: Boolean = false
  val inner: Inner = new Inner()
  var questions = List()
  var answers = List()

  override def toString : String = {
    val result_string = List(this.name, this.isAnonymous.toString, this.viewType, this.startTime, this.stopTime)
      .mkString(", ")
    result_string
  }
}
