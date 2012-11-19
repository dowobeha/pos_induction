package induction

import induction.mutable.ConditionalProbabilityDistribution
import induction.mutable.PriorProbabilityDistribution
import induction.mutable.Vocabulary

class HMM(textFile:String,val numHiddenStates:Int) {

	val vocab = new Vocabulary
    
    for (line <- scala.io.Source.fromFile(textFile,"UTF-8").getLines) {
      for (word <- line.split("""\s+""")) {
    	vocab.getInt(word)
      }
    }

    val π = PriorProbabilityDistribution.uniform(numHiddenStates)
    val α = ConditionalProbabilityDistribution.uniform(numHiddenStates,numHiddenStates)
    val β = ConditionalProbabilityDistribution.uniform(vocab.size,numHiddenStates)
  
    
  
}