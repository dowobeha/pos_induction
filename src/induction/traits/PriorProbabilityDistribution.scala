package induction.traits

trait PriorProbabilityDistribution {
	
  val variableVocabSize: Int
  val probabilities : Array[BigDecimal]
  
  def apply(variable:Int) : BigDecimal = {
    return probabilities(variable)
  }
  
  override def toString = toString(None)
  
  def toString(variableVocab:Option[Vocabulary]) : String = {
    val s = new StringBuilder
    
    for (v <- 0 until variableVocabSize) {
    	val p : BigDecimal = this.apply(v)
    	s.append(toString(v,p,variableVocab))
    	s.append('\n')
    }
    
    return s.toString
  }
  
  def toString(variable:Int, probability:BigDecimal, variableVocab:Option[Vocabulary]=None) : String = {
    val variableString = variableVocab match {
      case None => variable.toString
      case Some(vocab) => vocab.getString(variable)
    }

    return "P( "+variableString+" ) = " + probability.toString
  }
  
}
