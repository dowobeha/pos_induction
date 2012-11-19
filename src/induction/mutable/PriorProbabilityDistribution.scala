package induction.mutable

import scala.collection.mutable.ArrayBuffer

class PriorProbabilityDistribution(val variableVocabSize:Int) extends induction.traits.PriorProbabilityDistribution {

  val probabilities = new Array[Double](variableVocabSize)
  
  def update(variable:Int, probability:Double) : Unit = {
    probabilities(variable) = probability
  }
  
  def setUniform(probability:Double) {
    for (i <- 0 until probabilities.size) { 
    	probabilities(i) = probability
  	}
  }

}



object PriorProbabilityDistribution {
  
  def uniform(variableVocabSize:Int) : PriorProbabilityDistribution = {
    val d = new PriorProbabilityDistribution(variableVocabSize)
    
    d.setUniform(1.0 / variableVocabSize.toDouble)
    
    return d
  }
  
}

