package ch.ethz.ir.g19

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.PriorityQueue
import ch.ethz.dal.tinyir.alerts.Query
import ch.ethz.dal.tinyir.lectures.PrecisionRecall

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

  override def toString() = {
    val l = ArrayBuffer[List[Tuple2[Double, String]]]()
    r.foreach(pq => l.append(pq.toList))
    val sortedl = l.map(_.sortBy(x => -x._1))
    sortedl.toList.mkString("", "\n", "")
  }

  def outputMetrics(relevant: Set[String]): (Double, Double, Double, Double) = {
    var precRec =  List[(Double, Double)]()
    for (resultQuery <- r) {
      val retrieved = resultQuery.map(_._2).toSet
      val pr = PrecisionRecall.evaluate(retrieved, relevant)
      val tuple = (pr.precision, pr.recall)
      precRec ::= tuple
    }
    
    val f1List = precRec.map(t => (2.0*(t._1*t._2))/(t._1+t._2))
    val avgP = (precRec.map(_._1).sum / precRec.size)
    val avgR = (precRec.map(_._2).sum / precRec.size)
    val avgF1 = (f1List.sum/f1List.size)
    val MAP = avgF1.toDouble / r.size
    
    return (avgP, avgR, avgF1, MAP)
  }
}


