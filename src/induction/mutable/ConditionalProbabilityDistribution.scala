package induction.mutable

import scala.collection.mutable.ArrayBuffer

class ConditionalProbabilityDistribution(val variableVocabSize:Int, val givenVariableVocabSize:Int) extends induction.traits.ConditionalProbabilityDistribution {

  val probabilities = new Array[BigDecimal](variableVocabSize * givenVariableVocabSize)
  
  this.setUniform(0.0)
  
  def update(givenAnotherVariable:Int, variable:Int, probability:BigDecimal) : Unit = {
      val index = getIndexFor(variable,givenAnotherVariable)
    probabilities(index) = probability
  }
  
  def setUniform {
    setUniform(BigDecimal(1) / BigDecimal(variableVocabSize))
  }
  
  def setUniform(probability:BigDecimal) {
    for (i <- 0 until probabilities.size) { 
    	probabilities(i) = probability
  	}
  }

  def immutable = new induction.immutable.ConditionalProbabilityDistribution(this)
  
}



object ConditionalProbabilityDistribution {
  
  def uniform(variableVocabSize:Int, givenVariableVocabSize:Int) : ConditionalProbabilityDistribution = {
    val d = new ConditionalProbabilityDistribution(variableVocabSize,givenVariableVocabSize)
    
    d.setUniform(BigDecimal(1) / BigDecimal(variableVocabSize))
    
    return d
  }
  
}

