package interaction

import commands.Command
import exceptions.ParserException

import scala.io.Source
import scala.util.Try



object Reader {
  private def converter(name: String) : List[String] = {
    Source.fromFile(name).getLines().toList
  }

  def createCommands(querry : String) : Try[Command] = {
    Try {
      val charSet = querry.replaceAll("\\(\\(", "").replaceAll("\\)\\)", "").split("")
      val open_br = charSet.filter(c => c.equals("(")).toList
      val closed_br = charSet.filter(c => c.equals(")")).toList
      if (open_br.lengthCompare(closed_br.length) != 0) throw new ParserException()
      val pattern1 = "(/[^ ]+)".r
      val pattern2 = "\\((.*?(\\))+)*[^()]*\\)".r
      val pattern3 = "\n(\t| )*(.*)\n??".r

      val name = Try((pattern1 findAllIn querry).group(1))

      val params = Try((pattern2 findAllIn querry)
        .mkString("#")
        .split("#")
        .map(s => {
          s.drop(1)
            .dropRight(1)
            .replace("((", "(")
            .replace("))", ")")
        }).toList)

      val answers = Try((pattern3 findAllIn querry)
        .group(2)
        .mkString("#")
        .split("#")
        .map(s => {
          s.replace("((", "(")
            .replace("))", ")")
        }).toList)
      new Command(name, params, answers)
    }
  }

  def parse(name : String) : Try[Command] = {
    val list = converter(name)
    val result = list.mkString(util.Properties.lineSeparator)
    print(result)
    createCommands(result)
  }
}