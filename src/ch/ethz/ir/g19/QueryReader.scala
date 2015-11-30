package ch.ethz.ir.g19

import io.Source
import ch.ethz.dal.tinyir.alerts._

object QueryReader {
  def readQuery(path: String): List[Tuple2[Int, Query]] = {
    val nums = Source.fromFile(path)
        .getLines
        .filter(_.startsWith("<num>"))
        .map(l => l.split("Number:\\s+").last.trim.toInt)
        .toList
    val queries = Source.fromFile(path)
        .getLines
        .filter(_.startsWith("<title>"))
        .map(l => new Query(l.split("Topic:\\s+").last.trim))
        .toList
    nums.zip(queries)
  }
}
