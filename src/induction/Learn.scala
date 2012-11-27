package induction

import induction.hmm.ModelParameters
import induction.hmm.HMM
import induction.mutable.ConditionalProbabilityDistribution
import induction.mutable.PriorProbabilityDistribution
import induction.mutable.Vocabulary
import scala.collection.mutable.ArrayBuffer

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
    
    val hmms = new ArrayBuffer[HMM]
    
    for (line <- scala.io.Source.fromFile(textFile,"UTF-8").getLines) {
//      println(line)
//      val hmm = new HMM(line,λ)
//      println("Created hmm")
//      println(hmm.α)
//      println(hmm.β)
        hmms.append(new HMM(line,λ))
    }
    
    val reestimated_λ = ModelParameters.reestimate(λ, hmms)
    
    hmms.foreach(hmm => {
//        println(hmm.sentence)
        val α     = hmm.α
        val new_α = new HMM(hmm.sentence, reestimated_λ).α
        println("Probability of sentence: " + α + " → " + new_α)
    })
    
  }

}
