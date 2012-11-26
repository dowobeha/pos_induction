package induction

import induction.hmm.ModelParameters
import induction.hmm.HMM
import induction.mutable.ConditionalProbabilityDistribution
import induction.mutable.PriorProbabilityDistribution
import induction.mutable.Vocabulary

object Learn {

  def main(args: Array[String]): Unit = {
        
    val textFile = args(0)
    val numHiddenStates = args(1).toInt

	/** Vocabulary of observation symbols */
  	val V = new Vocabulary()

	// Read vocabulary of observation symbols from text file
	for (line <- scala.io.Source.fromFile(textFile,"UTF-8").getLines) {
		for (word <- line.split("""\s+""")) {
			V.getInt(word)
		}
    }
    
    val λ = ModelParameters.uniform(V.immutable,numHiddenStates)
    
    for (line <- scala.io.Source.fromFile(textFile,"UTF-8").getLines) {
      println(line)
      val hmm = new HMM(line,λ)
      println("Created hmm")
      println(hmm.α)
      println(hmm.β)
    }
    
  }

}
