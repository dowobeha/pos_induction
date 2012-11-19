package induction.immutable

class ConditionalProbabilityDistribution(d:induction.mutable.ConditionalProbabilityDistribution) extends induction.traits.ConditionalProbabilityDistribution {

  val probabilities = d.probabilities
  val variableVocabSize = d.variableVocabSize
  val givenVariableVocabSize = d.givenVariableVocabSize
  
}
