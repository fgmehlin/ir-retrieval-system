package ch.ethz.dal.tinyir.processing

object StopWords {
  val stopWords = Set("", "a", "i", "at", "by", "it", "in", "is", "of", "on",
                      "or", "to", "and", "are", "for", "the", "from", "that",
                      "with")
  def filter(tokens : Seq[String]) = tokens.filter(stopWords)
  def remove(tokens : Seq[String]) =
    tokens.filter(!stopWords.contains(_)).toList
}