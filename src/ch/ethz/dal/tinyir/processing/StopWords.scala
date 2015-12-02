package ch.ethz.dal.tinyir.processing

object StopWords {
  val stopWords = Set("", "-", "a", "i", "at", "by", "it", "in", "is", "of",
                      "on", "or", "to", "and", "are", "for", "the", "from",
                      "that", "with", "&", "so", "mr", "mrs", "be", "have", "can",
                      "cannot", "can't", "could", "couldn't", "did",
                      "didn't", "do", "does", "doesn't", "don't", "got", "gotten",
                      "his", "her", "some", "any", "will", "won't", "who", "what",
                      "how")
  def filter(tokens : Seq[String]) = tokens.filter(stopWords)
  def remove(tokens : Seq[String]) =
    tokens.filter(!stopWords.contains(_)).toList
}