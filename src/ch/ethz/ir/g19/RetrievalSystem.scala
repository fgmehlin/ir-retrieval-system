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
  val cfs = collection.mutable.Map[String, Int]()

  var corpusSize = 0
  val n = 5
  var docminlen = Integer.MAX_VALUE
  var docmaxlen = 0

  def main(args: Array[String]) {
    val originalQueries = List(//new Query("More than 150 former officers "/* + "of theoverthrown South Vietnamese government have been released from are-education camp after 13 years of detention, the official VietnamNews Agency reported Saturday.   The report from Hanoi, monitored in Bangkok, did not givespecific figures, but said those freed Friday included anex-Cabinet minister, a deputy minister, 10 generals, 115field-grade officers and 25 chaplains.   It quoted Col. Luu Van Ham, director of the Nam Ha camp south ofHanoi, as saying all 700 former South Vietnamese officials who hadbeen held at the camp now have been released.   They were among 1,014 South Vietnamese who were to be releasedfrom re-education camps under an amnesty announced by the Communistgovernment to mark Tet, the lunar new year that begins Feb. 17.   The Vietnam News Agency report said many foreign journalists anda delegation from the Australia-Vietnam Friendship"*/),
      new Query("new, campaign, aims, turn, tables, coke's, centralargument, fast-food, chains, shouldn't, sell, pepsi, fromtheir, fountain, spigots, because, effectively, supports, acompetitor, pepsi, --, owner, pizza, hut, taco, bell, andkentucky, fried, chicken, --, also, nation's, largestrestaurant, operator, as, one, recent, coke, ad, puts, if, apepsico, restaurant, your, competition, every, time, you, servepepsi, you're, pouring, money, into, your, competitor's, pocket, pepsi's, new, ad, which, being, run, major, restauranttrade, magazine, suggests, coke, itself, hardly, withoutvested, interests, restaurants, calling, coke, mccoke, thead, points, out, mcdonald's, coke's, biggest, customer, addressing, restaurant, operators, ad, says, your, biggestcompetitor, mcdonald's,  coke, mcdonald's, no, 1, ally, effort, win, customers, you, coke, pepsi, ad, says, we, doubt, they'll, help, yousucceed, mcdonald's, expense, final, zinger, to, coke, if, you're, not, mcdonald's, your, business, may, mean, mcnothing,  responds, coke, pepsi, trying, do, anything, can, toovercome, harsh, reality, no, business, has, ever, beensuccessful, long, competing, its, own, customers, ad, campaign, follows, coke's, virtual, gutting, pepsi'smajor, fountain, customers, past, two, weeks, coke, hasswiped, pepsi's, biggest, fountain, client, grand, metropolitanplc's, burger, king, corp, unit, has, won, major, accounts, fromtw, services, inc, food-service, company, largestfranchisee, hardee's, food, systems, inc, unit, imascoltd, earlier, this, year, wendy's, international, inc, announcedit, would, put, coke, instead, pepsi, its, company-ownedrestaurants, starting, 1991, pepsi's, fountain, thrust, has, been, blunted, says, emanuelgoldman, painewebber, inc, analyst, fountain, war, generally, more, important, volumeand, visibility, than, profitability, major, accounts, thetwo, rivals, compete, so, fiercely, contracts, theirprofit, margins, typically, less, generous, than, usual, beverage, analysts, say, but, when, customer, scans, orderboards, mcdonald's, no, 1, fast-food, chain, runsjoint, advertising, promotions, coke, soft-drinkcompany, gains, invaluable, exposure, loss, visibility, will, hurt, pepsi's, efforts, narrowcoke's, fountain, lead, starting, out, soda-fountainbusiness, more, than, century, ago, coca-cola, has, enjoyedstrong, dominance, area, ever, since, coke, currentlycontrols, about, 60%, fountain, business, --, more, thandouble, pepsi's, share, but, during, 1980s, pepsi, had, startedto, make, significant, inroads, into, fountain, market, particularly, winning, burger, king, contract, as, those, gains, now, unravel, some, beverage, analysts, areskeptical, about, appeal, pepsi's, new, campaign, i, thinkthe, fountain, business, important, coke, they, will, dowhatever, they, can, win, new, accounts, says, michael, bellas, president, beverage, marketing, corp, i, think, they'll, becompetitive, right, down, line, {on, smaller, restaurants}, withpepsi, but, clearly, mcdonald's, also, important, coke, itaccounts, about, 5%, all, coke's, u, s, soft-drink, volume, about, 15%, its, fountain, business, according, topainewebber, when, mcdonald's, opened, its, first, restaurantin, soviet, union, donald, r, keough, coca-cola's, president, flew, moscow, be, hand, festivities"),
      //new Query("ripples Iowa poll drug"),
      //new Query("drug ripples Guadalajara"),
      new Query("pepsi"),
      new Query("has"))
    val queries = originalQueries.map(q => normalize(q.qterms))

    val tfidfRanking = new Ranking(n, queries.size)
    val mleRanking = new Ranking(n, queries.size)

    val path = "tipster/zips"
    val qrelsPath = "tipster/qrels"

    // First pass
   
    var corpus = new TipsterCorpusIterator(path)
    while (corpus.hasNext && corpusSize < 25) {
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
    while (corpus.hasNext && corpusSize < 25) {
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
    println(tfidfRanking.r)
    println(mleRanking.r)
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
    val scores = ListBuffer[Double]()
    for (q <- queries) {
      scores += q.map(qword =>
        logtfs.getOrElse(qword, 0.0) * idfs.getOrElse(qword, 0.0)).sum
    }
    scores.toList
  }

  def MLE(tfs: Map[String, Int], queries: List[List[String]]) = {
    val scores = ListBuffer[Double]()
    val sumtf = tfs.values.sum.toDouble
    val lambda = Math.max(1 - (tfs.keys.size.toDouble - docminlen) / (docmaxlen - docminlen), 0.1)
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
