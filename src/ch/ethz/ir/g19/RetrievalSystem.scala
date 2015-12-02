package ch.ethz.ir.g19

import ch.ethz.dal.tinyir.alerts._
import ch.ethz.dal.tinyir.io._
import ch.ethz.dal.tinyir.lectures._
import ch.ethz.dal.tinyir.processing._
import io.Source

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.{ Map => MutMap }

object RetrievalSystem {
  var corpusSize = 0
  val n = 100
  
  
  def main(args: Array[String]) {
    var docminlen = Integer.MAX_VALUE // minimum length document
    var docmaxlen = 0 // maximum length document
    

    val tipsterPath = "tipster/zips" // 
    val qrelsPath = "tipster/qrels"
    val topicsPath = "tipster/topics_full" // topics with all the queries (51-100)
   
      
    val tipsterQueries = QueryReader.readQuery(topicsPath)
    val queries = tipsterQueries.map(q => normalize(q._1.qterms))

    val tfidfRanking = new Ranking(n, queries.size)
    val mleRanking = new Ranking(n, queries.size)

    val start = System.currentTimeMillis()
    
    // First pass
    var corpus = new TipsterCorpusIterator(tipsterPath)
    while (corpus.hasNext) {
      val doc = corpus.next
      val tokens = normalize(doc.tokens)
      val tf = TermFrequencies.tf(tokens)
      RelevanceModels.dfStore(tf)
      RelevanceModels.cfStore(tf)
      docminlen = Math.min(docminlen, tf.values.sum)
      docmaxlen = Math.max(docmaxlen, tf.values.sum)
      corpusSize += 1
    }

    val idfs = TermFrequencies.idf(RelevanceModels.dfs.toMap, corpusSize)
    RelevanceModels.sumCfs()

    // Second pass
    corpusSize = 0
    corpus = new TipsterCorpusIterator(tipsterPath)
    while (corpus.hasNext) {
      val doc = corpus.next
      val tokens = normalize(doc.tokens)
      // tfidf
      val logtfs = TermFrequencies.logtf(tokens)
      tfidfRanking.processScores(doc.name,
          RelevanceModels.tfidf(logtfs, idfs, queries))
      // mle
      val tfs = TermFrequencies.tf(tokens)
      mleRanking.processScores(doc.name,
          RelevanceModels.mle(tfs, queries, docminlen, docmaxlen))
      corpusSize += 1
    }


    val stop = System.currentTimeMillis()
    println("Time Elapsed : " + (stop-start).toDouble / (1000*60) + " Minutes")

    val queryids = tipsterQueries.map(_._2)
    val relevantDocs = QueryReader.readQrels(qrelsPath)
    println("TF-IDF RANKING")
    tfidfRanking.printSubmission(queryids)
//    tfidfRanking.printMetrics(queryids, relevantDocs)
    println("MLE RANKING")
    mleRanking.printSubmission(queryids)
//    mleRanking.printMetrics(queryids, relevantDocs)
    
  }

  def normalize(tokens: List[String]) =
    StopWords.remove(tokens.map(_.toLowerCase))
    
}
