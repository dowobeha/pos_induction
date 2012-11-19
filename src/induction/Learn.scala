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
    
    val λ = new ModelParameters(textFile,numHiddenStates)
    
    for (line <- scala.io.Source.fromFile(textFile,"UTF-8").getLines) {
      println(line)
      val hmm = new HMM(line,λ)
      println("Created hmm")
      println(hmm.α)
      println(hmm.β)
    }
    
  }

}
