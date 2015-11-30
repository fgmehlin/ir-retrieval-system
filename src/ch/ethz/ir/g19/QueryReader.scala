package ch.ethz.ir.g19

import io.Source
import ch.ethz.dal.tinyir.alerts._

/**
 * @author florangmehlin
 */
object QueryReader {
  var queries = List[Query]()
  def readQuery(path: String): List[Query] = {
    val topicRegex = ""
    for (line <- Source.fromFile(path).getLines()) {
      if (line.contains("<title>")) {
        val lineArray = line.split(":\\s+")
        queries ::= new Query(lineArray.apply(1))
      }

    }
    return queries
  }
}