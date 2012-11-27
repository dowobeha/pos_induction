package induction.mutable

import induction.math.Probability
import scala.collection.mutable.ArrayBuffer

class ConditionalProbabilityDistribution(val variableVocabSize:Int, val givenVariableVocabSize:Int) extends induction.traits.ConditionalProbabilityDistribution {

  val probabilities = new Array[Probability](variableVocabSize * givenVariableVocabSize)
  
  this.setUniform(new Probability(0))
  
  def update(givenAnotherVariable:Int, variable:Int, probability:Probability) : Unit = {
      val index = getIndexFor(variable,givenAnotherVariable)
    probabilities(index) = probability
  }
  
  def setUniform {
    setUniform(new Probability(1,variableVocabSize))
  }
  
  def setUniform(probability:Probability) {
    for (i <- 0 until probabilities.size) { 
    	probabilities(i) = probability
  	}
  }

  def immutable = new induction.immutable.ConditionalProbabilityDistribution(this)
  
}



object ConditionalProbabilityDistribution {
  
  def uniform(variableVocabSize:Int, givenVariableVocabSize:Int) : ConditionalProbabilityDistribution = {
    val d = new ConditionalProbabilityDistribution(variableVocabSize,givenVariableVocabSize)
    
    d.setUniform(new Probability(1,variableVocabSize))
    
    return d
  }
  
}

