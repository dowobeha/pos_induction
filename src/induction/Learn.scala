package induction

import induction.mutable.ConditionalProbabilityDistribution
import induction.mutable.PriorProbabilityDistribution
import induction.mutable.Vocabulary

object Learn {

  def main(args: Array[String]): Unit = {
    
    
    val textFile = args(0)
    val numHiddenStates = args(1).toInt
    
    val hmm = new HMM(textFile,numHiddenStates)
    
    //println(hmm.β.toString(Some(hmm.vocab),None))
    
    //println(hmm.α.toString(None,None))

    //println(hmm.π.toString(None))
    
  }

}
