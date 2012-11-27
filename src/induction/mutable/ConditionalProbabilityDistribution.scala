package induction.mutable

import scala.collection.mutable.ArrayBuffer

class ConditionalProbabilityDistribution(val variableVocabSize:Int, val givenVariableVocabSize:Int) extends induction.traits.ConditionalProbabilityDistribution {

  val probabilities = new Array[Double](variableVocabSize * givenVariableVocabSize)
  
  this.setUniform(0.0)
  
  def update(givenAnotherVariable:Int, variable:Int, probability:Double) : Unit = {
      val index = getIndexFor(variable,givenAnotherVariable)
    probabilities(index) = probability
  }
  
  def setUniform {
    setUniform(1.0 / variableVocabSize.toDouble)
  }
  
  def setUniform(probability:Double) {
    for (i <- 0 until probabilities.size) { 
    	probabilities(i) = probability
  	}
  }

  def immutable = new induction.immutable.ConditionalProbabilityDistribution(this)
  
}



object ConditionalProbabilityDistribution {
  
  def uniform(variableVocabSize:Int, givenVariableVocabSize:Int) : ConditionalProbabilityDistribution = {
    val d = new ConditionalProbabilityDistribution(variableVocabSize,givenVariableVocabSize)
    
    d.setUniform(1.0 / variableVocabSize.toDouble)
    
    return d
  }
  
}

