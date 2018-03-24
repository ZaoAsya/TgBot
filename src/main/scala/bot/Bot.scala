package bot

import commands.Command
import interaction.{Reader, Writer}

import scala.collection.mutable

object Bot extends App {
  val comDict = new mutable.HashMap[String, Command]()

  override def main(args: Array[String]) {
    val cmd = Reader.parse("in.txt")
    if (cmd.isSuccess) Writer.write(cmd.get.execute()) else Writer.write("Can't understand you! Speak slower!")
//    while (true) {
//      val cmd = Reader.parse("in.txt")
//      if (cmd.isSuccess) Writer.write(cmd.get.execute()) else Writer.write("Can't understand you! Speak slower!")
//    }
  }
}