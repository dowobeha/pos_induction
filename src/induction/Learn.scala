package induction

import grid.JPPF
import induction.hmm.ModelParameters
import induction.hmm.HMM
import induction.math.Math
import induction.math.Probability
import induction.mutable.ConditionalProbabilityDistribution
import induction.mutable.PriorProbabilityDistribution
import induction.mutable.Vocabulary
import scala.collection.mutable.ArrayBuffer
import scala.math.log
import scala.math.pow
import scala.math.E

object Learn {

  def main(args: Array[String]): Unit = {
        
    val textFile = args(0)
    val numHiddenStates = args(1).toInt
//    val newMethod = args(2)=="new"

    System.err.println("Reading sentences from " + textFile)    
    val sentences = new ArrayBuffer[String]
    for (line <- scala.io.Source.fromFile(textFile,"UTF-8").getLines) {
        sentences.append(line)
    }
    
	/** Vocabulary of observation symbols */
  	val V = new Vocabulary()

	// Read vocabulary of observation symbols from text file
    System.err.println("Initializing vocabulary")
	for (line <- sentences) {
		for (word <- line.split("""\s+""")) {
			V.getInt(word)
		}
    }
    
  	System.err.println("Initializing uniform model parameters")
    val λ = ModelParameters.uniform(V.immutable,numHiddenStates)
    


    
//    var λ_before = λ
//    
    var Δ = Double.PositiveInfinity
//    
//    while (Δ > 25) {
        
//	    val reestimated_λ = ModelParameters.jppfReestimate(λ, hmms)
    System.err.println("Reestimating model parameters")
    val reestimated_λ = ModelParameters.jppfReestimate(λ, sentences) 
//	        if (newMethod) {
//	            ModelParameters.alternateReestimate(λ, hmms) 
//	        } else {
//	        	ModelParameters.reestimate(λ, hmms)   
//	        }

        System.err.println("Constructing HMMs")
    val hmms = new ArrayBuffer[HMM]   
    		for (line <- sentences) {
    			hmms.append(new HMM(line,λ))
    		}

    hmms.foreach( hmm => {
    	if (hmm.α.toBigDecimal.compareTo(Probability.ZERO.toBigDecimal) == 0) {
    		System.err.println("Zero probability sentence:\t" + hmm.sentence)
    	}
    })

    System.err.println("Calculating original corpus log probability")
    
	    def Σ_hmm(f:(HMM => Double)) = Math.summation(hmms,f)
	
	    val corpus_wordcount           = Math.summation(hmms, (hmm:HMM) => hmm.T )    
	    
	    val corpus_logprob             = Σ_hmm( hmm => hmm.α.logProb )

	    System.err.println("Updating HMMs with new λ")
	    
	    for (i <- 0 until hmms.size) {
	        hmms(i) = new HMM(hmms(i).sentence, reestimated_λ)
	    }
	System.err.println("Calculating updated corpus log probability")
        
	    val reestimated_corpus_logprob = Σ_hmm( hmm => new HMM(hmm.sentence, reestimated_λ).α.logProb )
	
	    println("Corpus (base e) logprob:    " + corpus_logprob + " → " + reestimated_corpus_logprob)
	
	    val corpus_perplexity              =  pow( 10,             -corpus_logprob / corpus_wordcount )
	    val reestimated_corpus_perplexity  =  pow( 10, -reestimated_corpus_logprob / corpus_wordcount )
	    
	    Δ = ((corpus_perplexity - reestimated_corpus_perplexity) / corpus_perplexity ) * 100
	    
	    println("Corpus (base e) perplexity: " + corpus_perplexity + " → " + reestimated_corpus_perplexity)
	    println("Corpus (base e) perplexity: " + Δ + "% reduction")
	    
//    }    
    
	    JPPF.close
  }

}
