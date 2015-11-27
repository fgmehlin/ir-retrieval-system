package ch.ethz.ir.g19

import ch.ethz.dal.tinyir.alerts._
import ch.ethz.dal.tinyir.io._
import ch.ethz.dal.tinyir.lectures._
import ch.ethz.dal.tinyir.processing._
import io.Source

import scala.collection.mutable.{ Map => MutMap }
import scala.collection.mutable.ListBuffer

object RetrievalSystem {
  val dfs = collection.mutable.Map[String, Int]()
  // Temporary => store in file
  val cfs = collection.mutable.Map[String, Int]()
  val tfs = collection.mutable.Map[String, Map[String, Int]]()

  var corpusSize = 0
  val n = 20
  var dmin = 99999999
  var dmax = 0

  def main(args: Array[String]) {
    val originalQueries = List(new Query("More than 150 former officers "/* + "of theoverthrown South Vietnamese government have been released from are-education camp after 13 years of detention, the official VietnamNews Agency reported Saturday.   The report from Hanoi, monitored in Bangkok, did not givespecific figures, but said those freed Friday included anex-Cabinet minister, a deputy minister, 10 generals, 115field-grade officers and 25 chaplains.   It quoted Col. Luu Van Ham, director of the Nam Ha camp south ofHanoi, as saying all 700 former South Vietnamese officials who hadbeen held at the camp now have been released.   They were among 1,014 South Vietnamese who were to be releasedfrom re-education camps under an amnesty announced by the Communistgovernment to mark Tet, the lunar new year that begins Feb. 17.   The Vietnam News Agency report said many foreign journalists anda delegation from the Australia-Vietnam Friendship"*/),
      /*new Query("new, campaign, aims, turn, tables, coke's, centralargument, fast-food, chains, shouldn't, sell, pepsi, fromtheir, fountain, spigots, because, effectively, supports, acompetitor, pepsi, --, owner, pizza, hut, taco, bell, andkentucky, fried, chicken, --, also, nation's, largestrestaurant, operator, as, one, recent, coke, ad, puts, if, apepsico, restaurant, your, competition, every, time, you, servepepsi, you're, pouring, money, into, your, competitor's, pocket, pepsi's, new, ad, which, being, run, major, restauranttrade, magazine, suggests, coke, itself, hardly, withoutvested, interests, restaurants, calling, coke, mccoke, thead, points, out, mcdonald's, coke's, biggest, customer, addressing, restaurant, operators, ad, says, your, biggestcompetitor, mcdonald's,  coke, mcdonald's, no, 1, ally, effort, win, customers, you, coke, pepsi, ad, says, we, doubt, they'll, help, yousucceed, mcdonald's, expense, final, zinger, to, coke, if, you're, not, mcdonald's, your, business, may, mean, mcnothing,  responds, coke, pepsi, trying, do, anything, can, toovercome, harsh, reality, no, business, has, ever, beensuccessful, long, competing, its, own, customers, ad, campaign, follows, coke's, virtual, gutting, pepsi'smajor, fountain, customers, past, two, weeks, coke, hasswiped, pepsi's, biggest, fountain, client, grand, metropolitanplc's, burger, king, corp, unit, has, won, major, accounts, fromtw, services, inc, food-service, company, largestfranchisee, hardee's, food, systems, inc, unit, imascoltd, earlier, this, year, wendy's, international, inc, announcedit, would, put, coke, instead, pepsi, its, company-ownedrestaurants, starting, 1991, pepsi's, fountain, thrust, has, been, blunted, says, emanuelgoldman, painewebber, inc, analyst, fountain, war, generally, more, important, volumeand, visibility, than, profitability, major, accounts, thetwo, rivals, compete, so, fiercely, contracts, theirprofit, margins, typically, less, generous, than, usual, beverage, analysts, say, but, when, customer, scans, orderboards, mcdonald's, no, 1, fast-food, chain, runsjoint, advertising, promotions, coke, soft-drinkcompany, gains, invaluable, exposure, loss, visibility, will, hurt, pepsi's, efforts, narrowcoke's, fountain, lead, starting, out, soda-fountainbusiness, more, than, century, ago, coca-cola, has, enjoyedstrong, dominance, area, ever, since, coke, currentlycontrols, about, 60%, fountain, business, --, more, thandouble, pepsi's, share, but, during, 1980s, pepsi, had, startedto, make, significant, inroads, into, fountain, market, particularly, winning, burger, king, contract, as, those, gains, now, unravel, some, beverage, analysts, areskeptical, about, appeal, pepsi's, new, campaign, i, thinkthe, fountain, business, important, coke, they, will, dowhatever, they, can, win, new, accounts, says, michael, bellas, president, beverage, marketing, corp, i, think, they'll, becompetitive, right, down, line, {on, smaller, restaurants}, withpepsi, but, clearly, mcdonald's, also, important, coke, itaccounts, about, 5%, all, coke's, u, s, soft-drink, volume, about, 15%, its, fountain, business, according, topainewebber, when, mcdonald's, opened, its, first, restaurantin, soviet, union, donald, r, keough, coca-cola's, president, flew, moscow, be, hand, festivities"),
     */ new Query("ripples Iowa poll drug"),
      new Query("drug ripples Guadalajara"))
    val queries = originalQueries.map(q => normalize(q.qterms))

    val tfidfRanking = new Ranking(n, queries.size)

    val path = "tipster/zips"
    val qrelsPath = "tipster/qrels"

    // First pass
   
    var corpus = new TipsterCorpusIterator(path)
    while (corpus.hasNext /*&& corpusSize < 3*/) {
      val doc = corpus.next
      println(corpusSize)
      val tokens = normalize(doc.tokens)
      if (dmin > tokens.size)
        dmin = tokens.size
      if (dmax < tokens.size)
        dmax = tokens.size

      val tf = TermFrequencies.tf(tokens)
      dfStore(tf)
//      tfStore(doc.name, tf) // #
      cfStore(tf) // #
      corpusSize += 1
    }
  //  MLE(queries)

    val idfs = TermFrequencies.idf(dfs.toMap, corpusSize)

    // Second pass
    corpusSize = 0
    corpus = new TipsterCorpusIterator(path)
    while (corpus.hasNext/* && corpusSize < 3*/) {
      val doc = corpus.next
      println(corpusSize)
      val tokens = normalize(doc.tokens)
      val logtfs = TermFrequencies.logtf(tokens)
      tfidfRanking.processScores(doc.name, scoreTFIDF(logtfs, idfs, queries))
      corpusSize += 1
    }

    println(tfidfRanking.r)
  }

  def normalize(tokens: List[String]) =
    StopWords.remove(tokens.map(_.toLowerCase))

  def dfStore(logtf: Map[String, Any]) =
    dfs ++= logtf.keySet.map(t => t -> (1 + dfs.getOrElse(t, 0)))

  def tfStore(doc: String, tf: Map[String, Int]) =
    tfs += (doc -> tf)

  def cfStore(tf: Map[String, Int]) =
    cfs ++= tf.keySet.map(t => t -> (cfs.getOrElse(t, 0) + tf(t)))

  /** Compute the tfidf score for all queries against a document */
  def scoreTFIDF(logtfs: Map[String, Double], idfs: Map[String, Double],
    queries: List[List[String]]) = {
    val scores = ListBuffer[Double]()
    for (q <- queries) {
      scores += q.map(qword =>
        logtfs.getOrElse(qword, 0.0) * idfs.getOrElse(qword, 0.0)).sum
    }
    scores.toList
  }

  def MLE(queries: List[List[String]]) = {
    val mleRanking = new Ranking(n, queries.size)
    var lambda = 0.0

    for ((doc, freqs) <- tfs) {
        val scores = ListBuffer[Double]()
      for (q <- queries) {
        
      //  println(q)
        lambda = 1.0 - (freqs.keySet.size).toDouble / (dmin + dmax)
     //   println("lambda = " + lambda)
     //   println(doc)
        var qScore = 0.0
        for (qw <- q) {
          val P_HATwd = freqs.getOrElse(qw, 0).toDouble / freqs.values.sum
          val Pw = cfs.getOrElse(qw, 0).toDouble / cfs.values.sum
          var brac = 0.0
          if (Pw != 0 && P_HATwd != 0)
            brac = Math.log(1.0 + ((1.0 - lambda).toDouble / lambda) * (P_HATwd.toDouble / Pw)) + Math.log(lambda)
          qScore += brac
        }
  
       // println(qScore)
        scores += qScore
      }
        mleRanking.processScores(doc, scores.toList)

    }
    println(mleRanking.r)
  }
}
