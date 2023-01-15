import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val source = Source.fromFile("Example.txt")
    for (line <- source.getLines()) {
        println(line)
    }
  }
}
