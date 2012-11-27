package induction

import induction.hmm.ModelParameters
import induction.hmm.HMM
import induction.mutable.ConditionalProbabilityDistribution
import induction.mutable.PriorProbabilityDistribution
import induction.mutable.Vocabulary
import induction.util.Math
import scala.collection.mutable.ArrayBuffer
import scala.math.log
import scala.math.pow
import scala.math.E

object Learn {

  def main(args: Array[String]): Unit = {
        
    val textFile = args(0)
    val numHiddenStates = args(1).toInt

	/** Vocabulary of observation symbols */
  	val V = new Vocabulary()

	// Read vocabulary of observation symbols from text file
	for (line <- scala.io.Source.fromFile(textFile,"UTF-8").getLines) {
		for (word <- line.split("""\s+""")) {
			V.getInt(word)
		}
    }
    
    val λ = ModelParameters.uniform(V.immutable,numHiddenStates)
    
    val hmms = new ArrayBuffer[HMM]
    
    for (line <- scala.io.Source.fromFile(textFile,"UTF-8").getLines) {
//      println(line)
//      val hmm = new HMM(line,λ)
//      println("Created hmm")
//      println(hmm.α)
//      println(hmm.β)
        hmms.append(new HMM(line,λ))
    }
    
    hmms.foreach( hmm => {
        if (hmm.α <= 0) {
            System.err.println("Zero probability sentence:\t" + hmm.sentence)
        }
    })
    
    val reestimated_λ = ModelParameters.reestimate(λ, hmms)
    
    def Σ_hmm(f:(HMM => Double)) = Math.summation(hmms,f)

    val corpus_wordcount           = Σ_hmm( hmm => hmm.T )    
    
    val corpus_logprob             = Σ_hmm( hmm => log(hmm.α) )
    val reestimated_corpus_logprob = Σ_hmm( hmm => log(new HMM(hmm.sentence, reestimated_λ).α) )
    println("Corpus logprob: " + corpus_logprob + " → " + reestimated_corpus_logprob)

    val corpus_perplexity              =  pow( E,             -corpus_logprob / corpus_wordcount )
    val reestimated_corpus_perplexity  =  pow( E, -reestimated_corpus_logprob / corpus_wordcount )
    
    println("Corpus perplexity: " + corpus_perplexity + " → " + reestimated_corpus_perplexity)
    
//    hmms.foreach(hmm => {
////        println(hmm.sentence)
//        val α     = hmm.α
//        val new_α = new HMM(hmm.sentence, reestimated_λ).α
//        println("Probability of sentence: " + α + " → " + new_α)
//    })
    
  }

}
