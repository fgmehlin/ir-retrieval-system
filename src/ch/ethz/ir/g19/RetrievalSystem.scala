package ch.ethz.ir.g19

import ch.ethz.dal.tinyir.alerts._
import ch.ethz.dal.tinyir.io._
import ch.ethz.dal.tinyir.lectures._
import ch.ethz.dal.tinyir.processing._
import com.github.aztek.porterstemmer._

import scala.collection.mutable.{ Map => MutMap }
import scala.collection.mutable.ListBuffer

object RetrievalSystem {
  val dfs = MutMap[String, Int]()
  val tfs = MutMap[String, Map[String, Double]]()
  val cfs = MutMap[String, Int]()
  var n = 0

  val tfidfScores = MutMap[String, List[Double]]()

  def main(args: Array[String]) {
    val originalQueries = List(new Query("new, campaign, aims, turn, tables, coke's, centralargument, fast-food, chains, shouldn't, sell, pepsi, fromtheir, fountain, spigots, because, effectively, supports, acompetitor, pepsi, --, owner, pizza, hut, taco, bell, andkentucky, fried, chicken, --, also, nation's, largestrestaurant, operator, as, one, recent, coke, ad, puts, if, apepsico, restaurant, your, competition, every, time, you, servepepsi, you're, pouring, money, into, your, competitor's, pocket, pepsi's, new, ad, which, being, run, major, restauranttrade, magazine, suggests, coke, itself, hardly, withoutvested, interests, restaurants, calling, coke, mccoke, thead, points, out, mcdonald's, coke's, biggest, customer, addressing, restaurant, operators, ad, says, your, biggestcompetitor, mcdonald's,  coke, mcdonald's, no, 1, ally, effort, win, customers, you, coke, pepsi, ad, says, we, doubt, they'll, help, yousucceed, mcdonald's, expense, final, zinger, to, coke, if, you're, not, mcdonald's, your, business, may, mean, mcnothing,  responds, coke, pepsi, trying, do, anything, can, toovercome, harsh, reality, no, business, has, ever, beensuccessful, long, competing, its, own, customers, ad, campaign, follows, coke's, virtual, gutting, pepsi'smajor, fountain, customers, past, two, weeks, coke, hasswiped, pepsi's, biggest, fountain, client, grand, metropolitanplc's, burger, king, corp, unit, has, won, major, accounts, fromtw, services, inc, food-service, company, largestfranchisee, hardee's, food, systems, inc, unit, imascoltd, earlier, this, year, wendy's, international, inc, announcedit, would, put, coke, instead, pepsi, its, company-ownedrestaurants, starting, 1991, pepsi's, fountain, thrust, has, been, blunted, says, emanuelgoldman, painewebber, inc, analyst, fountain, war, generally, more, important, volumeand, visibility, than, profitability, major, accounts, thetwo, rivals, compete, so, fiercely, contracts, theirprofit, margins, typically, less, generous, than, usual, beverage, analysts, say, but, when, customer, scans, orderboards, mcdonald's, no, 1, fast-food, chain, runsjoint, advertising, promotions, coke, soft-drinkcompany, gains, invaluable, exposure, loss, visibility, will, hurt, pepsi's, efforts, narrowcoke's, fountain, lead, starting, out, soda-fountainbusiness, more, than, century, ago, coca-cola, has, enjoyedstrong, dominance, area, ever, since, coke, currentlycontrols, about, 60%, fountain, business, --, more, thandouble, pepsi's, share, but, during, 1980s, pepsi, had, startedto, make, significant, inroads, into, fountain, market, particularly, winning, burger, king, contract, as, those, gains, now, unravel, some, beverage, analysts, areskeptical, about, appeal, pepsi's, new, campaign, i, thinkthe, fountain, business, important, coke, they, will, dowhatever, they, can, win, new, accounts, says, michael, bellas, president, beverage, marketing, corp, i, think, they'll, becompetitive, right, down, line, {on, smaller, restaurants}, withpepsi, but, clearly, mcdonald's, also, important, coke, itaccounts, about, 5%, all, coke's, u, s, soft-drink, volume, about, 15%, its, fountain, business, according, topainewebber, when, mcdonald's, opened, its, first, restaurantin, soviet, union, donald, r, keough, coca-cola's, president, flew, moscow, be, hand, festivities"),
      new Query("company profitability revenue earnings"), new Query("pepsi"))
    val queries = originalQueries.map(q => normalize(q.qterms))

    val path = "tipster/zips"
    val corpusFirstPass = new TipsterCorpusIterator(path)
    val corpusSecondPass = new TipsterCorpusIterator(path)

    while (corpusFirstPass.hasNext && n < 3) {
      val doc = corpusFirstPass.next
      val tokens = normalize(doc.tokens)
      val tf = TermFrequencies.tf(tokens)
      // ----------- document frequencies
      // val logtf = TermFrequencies.logtf(tokens)
      //logtfs += doc.name -> logtf
      df(tf)
      // ----------- MLE
      // val tf = TermFrequencies.tf(tokens)
      cfs ++= tf.keySet.map(t => t -> (1 + cfs.getOrElse(t, 0)))

      n += 1
    }
    var n2 = 0
    val idfs = TermFrequencies.idf(dfs.toMap, n)
    println("idfs")
    println(idfs)
    while (corpusSecondPass.hasNext && n2 < 3) {
      val doc = corpusSecondPass.next
      val tokens = normalize(doc.tokens)
      // ----------- tfidf
      tfidfScores += doc.name -> scoreTermBased(tokens, queries, idfs)
      //logtfs += doc.name -> logtf
      //df(logtf)

      // ----------- MLE

      /*val lambda = 0.5
      for (q <- queries) {
        for ((did, freqs) <- tfs) {
          val P_HATwd = q.map { qword => freqs.getOrElse(qword, 0.0) / freqs.values.sum }
          val Pw = q.map { qword => cfs.getOrElse(qword, 0).toDouble / cfs.values.sum }
          // (q.toSet & freqs.keySet).map { w => Math.log(1+((1.0-lambda)/lambda))*(P_HATwd/Pw) }

        }
      }*/
    n2+=1
    }
    //println(tfidfScores)
  }

  def normalize(tokens: List[String]) =
    StopWords.remove(tokens.map(_.toLowerCase))//.map { x => PorterStemmer.stem(x) }

  def df(logtf: Map[String, Any]) =
    dfs ++= logtf.keySet.map(t => t -> (1 + dfs.getOrElse(t, 0)))

  /** Computes the score for all the queries against one document */
  def scoreTermBased(tokens: List[String], queries: List[List[String]], idfs: Map[String, Double]) = {
    val logtfs = TermFrequencies.logtf(tokens)
    val currentScores = ListBuffer[Double]()
    println(idfs)
    for (q <- queries)
    //  println(q.map(wq => /*logtfs.getOrElse(wq, 0.0) * */ idfs.getOrElse(wq, 0.0)))
      //currentScores += q.map(wq => logtfs.getOrElse(wq, 0.0) * idfs.getOrElse(wq, 0.0)).sum
      
    currentScores.toList
  }

  def MLE(queries: List[List[String]]) = {
    val lambda = 0.5
    for (q <- queries) {
      for ((did, freqs) <- tfs) {
        val P_HATwd = q.map { qword => freqs.getOrElse(qword, 0.0) / freqs.values.sum }
        val Pw = q.map { qword => cfs.getOrElse(qword, 0).toDouble / cfs.values.sum }
        // (q.toSet & freqs.keySet).map { w => Math.log(1+((1.0-lambda)/lambda))*(P_HATwd/Pw) }

      }
    }
  }
}
