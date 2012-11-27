package induction.mutable

import scala.collection.mutable.ArrayBuffer

class PriorProbabilityDistribution(val variableVocabSize:Int) extends induction.traits.PriorProbabilityDistribution {

  val probabilities = new Array[BigDecimal](variableVocabSize)
  
  def update(variable:Int, probability:BigDecimal) : Unit = {
    probabilities(variable) = probability
  }
  
  def setUniform {
    setUniform(BigDecimal(1) / BigDecimal(variableVocabSize))
  }
  
  def setUniform(probability:BigDecimal) {
    for (i <- 0 until probabilities.size) { 
    	probabilities(i) = probability
  	}
  }

  def immutable = new induction.immutable.PriorProbabilityDistribution(this)
  
}



object PriorProbabilityDistribution {
  
  def uniform(variableVocabSize:Int) : PriorProbabilityDistribution = {
    val d = new PriorProbabilityDistribution(variableVocabSize)
    
    d.setUniform(BigDecimal(1) / BigDecimal(variableVocabSize))
    
    return d
  }
  
}

