package poll

case class Poll(title: String,
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
}
