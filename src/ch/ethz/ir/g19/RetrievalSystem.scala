package ch.ethz.ir.g19

import ch.ethz.dal.tinyir.alerts._
import ch.ethz.dal.tinyir.io._
import ch.ethz.dal.tinyir.lectures._
import ch.ethz.dal.tinyir.processing._
import io.Source
import breeze.linalg.DenseMatrix
import breeze.linalg.DenseVector

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.{ Map => MutMap }

object RetrievalSystem {
  var corpusSize = 0
  val n = 100

  def main(args: Array[String]) {
    var docminlen = Integer.MAX_VALUE // minimum length document
    var docmaxlen = 0 // maximum length document

    val tipsterPath = "tipster/zips"
    val qrelsPath = "tipster/qrels"
    val topicsPath = "tipster/topics"
    val glove6b50d = "glove/glove.6B.50d.txt"
    
    val gloveRowSize = Source.fromFile(glove6b50d)("UTF-8").getLines.size
    println(gloveRowSize)
    val gloveArray = Source.fromFile(glove6b50d)("UTF-8").getLines.toArray.flatMap(_.split(" ").drop(1)).map(_.toDouble)
    val gloveWordArray = Source.fromFile(glove6b50d)("UTF-8").getLines.toArray.flatMap(_.split(" ").take(1))
    val gloveMatrix = new DenseMatrix(gloveRowSize, 50, gloveArray)

    
    val testQueryWMD1 = new Query("Obama speaks to the media in Illinois")
    val testQueryWMD2 = new Query("The President greets the press in Chicago")
    

    
    
    val tipsterQueries = QueryReader.readQuery(topicsPath)
    val queries = tipsterQueries.map(q => normalize(q._1.qterms))

    val tfidfRanking = new Ranking(n, queries.size)
    val mleRanking = new Ranking(n, queries.size)

    val start = System.currentTimeMillis()
    // First pass
    var corpus = new TipsterCorpusIterator(tipsterPath)
    while (corpus.hasNext) {
      val doc = corpus.next
      print(corpusSize+"\r")
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
      print(corpusSize+"\r")
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
    println(tfidfRanking)
    println(mleRanking)

    val stop = System.currentTimeMillis()
    println("Time Elapsed : " + (stop-start).toDouble / (1000*60) + " Minutes")

    val queryids = tipsterQueries.map(_._2)
    val relevantDocs = QueryReader.readQrels(qrelsPath)
    tfidfRanking.printMetrics(queryids, relevantDocs)
    mleRanking.printMetrics(queryids, relevantDocs)
    
    
  }

  def normalize(tokens: List[String]) =
    StopWords.remove(tokens.map(_.toLowerCase))
}
