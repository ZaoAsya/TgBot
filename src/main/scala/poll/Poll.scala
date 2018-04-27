package poll

case class Poll(name: String,
                isAnonymous: Boolean,
                viewType: String,
                startTime: String,
                stopTime: String,
                isRun: Boolean = false,
                questions : List[String] = List(),
                answers : Map[Int, List[String]] = Map[Int, List[String]]())