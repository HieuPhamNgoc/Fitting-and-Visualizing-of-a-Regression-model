import scala.io.Source

abstract class Filereader(fileName: String) {
  def data: IndexedSeq[Seq[Double]]
}

class JsonReader(fileName: String) extends Filereader(fileName) {

  val untreatedText = Source.fromFile(fileName)
  var lines = Seq[String]()
  for (line <- untreatedText.getLines) {
    lines = lines :+ line
  }
  untreatedText.close
  lines = lines.filterNot(_ == "[").filterNot(_ == "]").filterNot(_.contains("{")).filterNot(_.contains("}")).map(line => line.dropWhile(_ != ':').drop(1).trim)
  for (i <- lines.indices) {
    if (lines(i).contains(",")) {
      lines = lines.updated(i, lines(i).dropRight(1))
    }
  }
  val result = lines.map(_.toDouble).sliding(2, 2).toIndexedSeq
  def data = this.result

}


class CSVReader(fileName: String) extends Filereader(fileName) {

  val untreatedText = Source.fromFile(fileName)
  var lines = Seq[String]()
  for (line <- untreatedText.getLines) {
    lines = lines :+ line
  }
  untreatedText.close
  val result = lines.drop(1).map(seq => Seq(seq.takeWhile(_ != ','), seq.dropWhile(_ != ',').drop(1).trim)).map(seq => seq.take(2)).toIndexedSeq.map(seq => seq.map(_.toDouble))
  def data = this.result
}