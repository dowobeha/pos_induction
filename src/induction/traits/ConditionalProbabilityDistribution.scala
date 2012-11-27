package induction.traits

import induction.math.Probability

trait ConditionalProbabilityDistribution {
	
  val variableVocabSize: Int
  val givenVariableVocabSize: Int
  val probabilities : Array[Probability]
  
  def apply(givenAnotherVariable:Int, variable:Int) : Probability = {
    return probabilities(getIndexFor(variable,givenAnotherVariable))
  }
  
  protected def getIndexFor(variable:Int, givenAnotherVariable:Int) : Int = {
    return givenAnotherVariable*variableVocabSize + variable
  }
  
  override def toString = toString(None,None)
  
  def toString(variableVocab:Option[Vocabulary], givenVariableVocab:Option[Vocabulary]) : String = {
    val s = new StringBuilder
    
    for (g <- 0 until givenVariableVocabSize) {
    	for (v <- 0 until variableVocabSize) {
    		val p : Probability = this.apply(v,g)
    		s.append(toString(v,g,p,variableVocab,givenVariableVocab))
    		s.append('\n')
    	} 
    }
    
    return s.toString
  }
  
  def toString(variable:Int, givenAnotherVariable:Int, probability:Probability, variableVocab:Option[Vocabulary]=None, givenVariableVocab:Option[Vocabulary]=None) : String = {
    val variableString = variableVocab match {
      case None => variable.toString
      case Some(vocab) => vocab.getString(variable)
    }
    
    val givenVariableString = givenVariableVocab match {
      case None => givenAnotherVariable.toString
      case Some(vocab) => vocab.getString(givenAnotherVariable)
    }
    
    return "P( "+variableString+" | " + givenVariableString + " ) = " + probability.toString
  }
  
}
