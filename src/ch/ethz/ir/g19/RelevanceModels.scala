package ch.ethz.ir.g19

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.{ Map => MutMap }

object RelevanceModels {
  val dfs = MutMap[String, Int]() // document frequencies
  val cfs = MutMap[String, Int]() // collection frequencies
  var sumcf = 0.0

  def dfStore(logtf: Map[String, Any]) =
    dfs ++= logtf.keys.map(t => t -> (1 + dfs.getOrElse(t, 0)))

  def cfStore(tf: Map[String, Int]) =
    cfs ++= tf.keySet.map(t => t -> (cfs.getOrElse(t, 0) + tf(t)))

  def sumCfs() = sumcf = cfs.values.sum.toDouble

  def mle(tfs: Map[String, Int], queries: List[List[String]], docminlen: Int,
      docmaxlen: Int) = {
    val scores = ArrayBuffer[Double]()
    val sumtf = tfs.values.sum.toDouble
    val lambda = Math.max(1 - (sumtf - docminlen) / (docmaxlen - docminlen), 0.1)
    for (q <- queries) {
      var ws = tfs.keySet & q.toSet
      val score = ws.map(w => Math.log10(1 + ((1.0 - lambda) / lambda))
                             * ((tfs.get(w).get / sumtf) / (cfs.get(w).get / sumcf))
                  + Math.log10(lambda)).sum
      scores += score
    }
    scores.toList
  }

  /** Compute the tfidf score for all queries against a document */
  def tfidf(logtfs: Map[String, Double], idfs: Map[String, Double],
    queries: List[List[String]]) = {
    val scores = ArrayBuffer[Double]()
    for (q <- queries) {
      scores += q.map(qword =>
        logtfs.getOrElse(qword, 0.0) * idfs.getOrElse(qword, 0.0)).sum
    }
    scores.toList
  }
}
