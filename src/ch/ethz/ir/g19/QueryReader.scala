package ch.ethz.ir.g19

import io.Source
import ch.ethz.dal.tinyir.alerts._

object QueryReader {
  def readQrels(path: String) = {
    Source.fromFile(path)
        .getLines
        .filter(_.endsWith("1"))
        .toList
        .map(_.split(" "))
        .groupBy(_.head)
        .mapValues(l => l.map(_.apply(2).replaceAll("-", "")).toSet)
  }

  def readQuery(path: String) = {
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
    queries.zip(nums)
  }
}
