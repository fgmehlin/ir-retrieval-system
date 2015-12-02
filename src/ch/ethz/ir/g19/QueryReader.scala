package ch.ethz.ir.g19

import io.Source
import ch.ethz.dal.tinyir.alerts._
import scala.collection.mutable.ArrayBuffer
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
    val queries = ArrayBuffer[Query]()
    var query = ""
    var topicFound = false

    val nums = Source.fromFile(path)
      .getLines
      .filter(_.startsWith("<num>"))
      .map(l => l.split("Number:\\s+").last.trim.toInt)
      .toList
      
      
    /* val queries = Source.fromFile(path)
        .getLines
        .filter(_.startsWith("<title>"))
        .map(l => new Query(v))
        .toList*/
      
      // As the queries could be written on more than one line, 
      // we had to change the above functional code to the below ugly one.
      
    for (line <- Source.fromFile(path).getLines()) {
      if (topicFound) {
        if (!line.isEmpty())
          query = query + " " + line.trim()
        else {
          queries += new Query(query)
          topicFound = false
        }
      }
      if (line.startsWith("<title>")) {
        query = line.split("Topic:\\s+").last.trim
        topicFound = true
      }
    }
    queries.toList.zip(nums)
  }
}
