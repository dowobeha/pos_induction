package induction.mutable

import induction.math.Probability
import scala.collection.mutable.ArrayBuffer

class PriorProbabilityDistribution(val variableVocabSize:Int) extends induction.traits.PriorProbabilityDistribution {

  val probabilities = new Array[Probability](variableVocabSize)
  
  def update(variable:Int, probability:Probability) : Unit = {
    probabilities(variable) = probability
  }
  
  def setUniform {
    setUniform(new Probability(1,variableVocabSize))
  }
  
  def setUniform(probability:Probability) {
    for (i <- 0 until probabilities.size) { 
    	probabilities(i) = probability
  	}
  }

  def immutable = new induction.immutable.PriorProbabilityDistribution(this)
  
}



object PriorProbabilityDistribution {
  
  def uniform(variableVocabSize:Int) : PriorProbabilityDistribution = {
    val d = new PriorProbabilityDistribution(variableVocabSize)
    
    d.setUniform(new Probability(1,variableVocabSize))
    
    return d
  }
  
}

