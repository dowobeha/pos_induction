package induction.immutable

class PriorProbabilityDistribution(d:induction.mutable.PriorProbabilityDistribution) extends induction.traits.PriorProbabilityDistribution {

    val variableVocabSize = d.variableVocabSize
	val probabilities     = d.probabilities
	
}