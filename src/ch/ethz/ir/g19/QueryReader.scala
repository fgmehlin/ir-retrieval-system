package ch.ethz.ir.g19

import io.Source
import ch.ethz.dal.tinyir.alerts._

object QueryReader {
  def readQuery(path: String): List[Query] = {
    Source.fromFile(path)
        .getLines
        .filter(_.startsWith("<title>"))
        .map(l => new Query(l.split("Topic:\\s+").apply(1).trim))
        .toList
  }
}
