package ch.ethz.ir.g19

import ch.ethz.dal.tinyir.alerts._
import ch.ethz.dal.tinyir.io._
import ch.ethz.dal.tinyir.lectures._
import ch.ethz.dal.tinyir.processing._
import io.Source

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.{ Map => MutMap }

object RetrievalSystem {
  val dfs = MutMap[String, Int]()
  val cfs = MutMap[String, Int]()
  var sumcf = 0.0

  var corpusSize = 0
  val n = 100
  var docminlen = Integer.MAX_VALUE
  var docmaxlen = 0

  def main(args: Array[String]) {
    val start = System.currentTimeMillis()

    val path = "tipster/zips"
    val qrelsPath = "tipster/qrels"
    val topicsQueries = "tipster/topics"

    val testQueries = QueryReader.readQuery(topicsQueries)
    val queries = testQueries.map(q => normalize(q._2.qterms)) 

    val tfidfRanking = new Ranking(n, queries.size)
    val mleRanking = new Ranking(n, queries.size)

    // Read qrels
    val relevantDocs = Source.fromFile(qrelsPath)
        .getLines
        .filter(_.endsWith("1"))
        .toList
        .map(_.split(" "))
        .groupBy(_.head)
        .mapValues(l => l.map(_.apply(2)).toSet)
    // First pass
    var corpus = new TipsterCorpusIterator(path)
    while (corpus.hasNext && corpusSize < 100) {
      val doc = corpus.next
      val tokens = normalize(doc.tokens)
      val tf = TermFrequencies.tf(tokens)
      dfStore(tf)
      cfStore(tf)
      docminlen = Math.min(docminlen, tf.size)
      docmaxlen = Math.max(docmaxlen, tf.size)
      corpusSize += 1
    }

    val idfs = TermFrequencies.idf(dfs.toMap, corpusSize)
    sumcf = cfs.values.sum.toDouble

    // Second pass
    corpusSize = 0
    corpus = new TipsterCorpusIterator(path)
    while (corpus.hasNext && corpusSize < 100) {
      val doc = corpus.next
      val tokens = normalize(doc.tokens)
      // tfidf
      val logtfs = TermFrequencies.logtf(tokens)
      tfidfRanking.processScores(doc.name, scoreTFIDF(logtfs, idfs, queries))
      // mle
      val tfs = TermFrequencies.tf(tokens)
      mleRanking.processScores(doc.name, MLE(tfs, queries))
      corpusSize += 1
    }
    println(tfidfRanking)
    println(mleRanking)

    val stop = System.currentTimeMillis()
    println("Time Elapsed : " + (stop-start).toDouble / (1000*60) + " Minutes")

    tfidfRanking.printMetrics(testQueries.map(_._1), relevantDocs)
    mleRanking.printMetrics(testQueries.map(_._1), relevantDocs)
  }

  def normalize(tokens: List[String]) =
    StopWords.remove(tokens.map(_.toLowerCase))

  def dfStore(logtf: Map[String, Any]) =
    dfs ++= logtf.keySet.map(t => t -> (1 + dfs.getOrElse(t, 0)))

  def cfStore(tf: Map[String, Int]) =
    cfs ++= tf.keySet.map(t => t -> (cfs.getOrElse(t, 0) + tf(t)))

  /** Compute the tfidf score for all queries against a document */
  def scoreTFIDF(logtfs: Map[String, Double], idfs: Map[String, Double],
    queries: List[List[String]]) = {
    val scores = ArrayBuffer[Double]()
    for (q <- queries) {
      scores += q.map(qword =>
        logtfs.getOrElse(qword, 0.0) * idfs.getOrElse(qword, 0.0)).sum
    }
    scores.toList
  }

  def MLE(tfs: Map[String, Int], queries: List[List[String]]) = {
    val scores = ArrayBuffer[Double]()
    val sumtf = tfs.values.sum.toDouble
    val lambda = 0.5//Math.max(1 - (tfs.keys.size.toDouble - docminlen) / (docmaxlen - docminlen), 0.1)
    for (q <- queries) {
      var ws = tfs.keySet & q.toSet
      val score = ws.map(w => Math.log10(1 + ((1.0 - lambda) / lambda))
                             * ((tfs.get(w).get / sumtf) / (cfs.get(w).get / sumcf))
                  + Math.log10(lambda)).sum
      scores += score
    }
    scores.toList
  }
}
