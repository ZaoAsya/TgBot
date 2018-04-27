package poll


case class Poll(id: Int, name: String,
                isAnonymous: Boolean,
                viewType: String,
                startTime: String,
                stopTime: String,
                isRun: Boolean = false,
                questions : Vector[(String, String)] = Vector(),
                answers : Vector[Vector[String]] = Vector())