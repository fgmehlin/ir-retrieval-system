package ch.ethz.dal.tinyir.processing

object Tokenizer {
  def tokenize (text: String) : List[String] =
    text.replaceAll("U.S.", "United States of America").split("[ /().,;:?!`\"\t\n\r\f]+").toList
    //text.split("\\W+").toList
}