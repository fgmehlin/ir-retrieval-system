package ch.ethz.ir.g19

import ch.ethz.dal.tinyir.io._
import java.io.File

object RetrievalSystem {
  
  
  def main(args: Array[String]) {
    val path = "tipster/zips"
    
//    println(new File(".").getAbsolutePath())
//    val docs = new ZipDirStream(path)
//    println("Reading from path = " + path)
//    println("Number of documents = " + docs.length)
//    val zipList = docs.ziplist
    
    val tipS = new TipsterStream(path)
    //tipS.
    val xmlDoc = tipS.stream.foreach { x => println(x.name) }
    

    
    
  }
}
