package ch.ethz.ir.g19

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.PriorityQueue
import ch.ethz.dal.tinyir.alerts.Query

object MinOrder extends Ordering[Tuple2[Double, String]] {
  def compare(x: Tuple2[Double, String], y: Tuple2[Double, String]) =
    y._1 compare x._1
}

/**
 * @param n number of items to keep in the ranking
 * @param nq number of queries
 */
class Ranking(n: Int, nq: Int) {
  val r = List.fill(nq)(PriorityQueue.empty(MinOrder))

  def printMetrics(queries: List[Int], relevant: Map[String, Set[String]]) = {
    val precisions =  ArrayBuffer[Double]()
    val recalls =  ArrayBuffer[Double]()
    val APs =  ArrayBuffer[Double]()
    val retrieved = sortByScore.map(l => l.map(_._2))
    for ((q, i) <- queries.zipWithIndex) {
      val currentRetrieved = retrieved.apply(i)
      val currentRelevant = relevant.get(q + "").get
      val TP = (currentRetrieved.toSet & currentRelevant).size.toDouble
      precisions += TP / currentRetrieved.size
      recalls += TP / currentRelevant.size
      val precision = currentRetrieved
          .zipWithIndex
          .filter{case (doc, _) => currentRelevant.contains(doc)}
          .zipWithIndex
          .map(t => (t._2 + 1.0) / (t._1._2 + 1.0))
      APs += precision.sum / Math.min(currentRelevant.size, 100)
    }
    val alpha = 0.5
    val avgP = (precisions.sum / precisions.size)
    val avgR = (recalls.sum / recalls.size)
    val fScores = precisions.zip(recalls)
        .map(t => 1.0/(alpha / t._1 + (1.0 - alpha) / t._2))
    val avgF = (fScores.sum / fScores.size)
    val MAP = APs.sum / queries.size

    println("P=" + avgP + " R=" + avgR + " F=" + avgF + " MAP=" + MAP)
  }

  def processScores(name: String, scores: List[Double]) = {
    for (i <- 0 until nq) {
      val pq = r.apply(i)
      val newTuple = Tuple2(scores.apply(i), name)
      if (pq.size < n) {
        pq.enqueue(newTuple)
      } else {
        val deq = pq.dequeue()
        pq.enqueue(List(deq, newTuple).maxBy(_._1))
      }
    }
  }

  /** Creates list of lists with the rankings sorted by score */
  def sortByScore() = {
    val l = ArrayBuffer[List[Tuple2[Double, String]]]()
    r.foreach(pq => l.append(pq.toList))
    l.map(_.sortBy(x => -x._1)).toList
  }

  override def toString() = {
    val sortedl = sortByScore
    sortedl.toList.mkString("\n")
  }
}
