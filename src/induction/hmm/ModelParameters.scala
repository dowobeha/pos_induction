package induction.hmm

import induction.mutable.ConditionalProbabilityDistribution
import induction.mutable.PriorProbabilityDistribution
import induction.mutable.Vocabulary

/**
 * Parameters for a Hidden Markov Model, following the terminology of Rabiner & Juang (1986).
 * 
 * @see Rabiner, L.R. and Juang, B.H. 
 *      An Introduction to Hidden Markov Models. 
 *      IEEE ASSP Magazine. January 1986. 
 */
class ModelParameters(textFile:String, numHiddenStates:Int) {

	/** Vocabulary of observation symbols */
  	val V = new Vocabulary()

	// Read vocabulary of observation symbols from text file
	for (line <- scala.io.Source.fromFile(textFile,"UTF-8").getLines) {
		for (word <- line.split("""\s+""")) {
			V.getInt(word)
		}
    }

  	/** Prior distribution over hidden states */
    private val Π = PriorProbabilityDistribution.uniform(numHiddenStates)
    
    /** Hidden state transition distribution */
    private val A = ConditionalProbabilityDistribution.uniform(numHiddenStates,numHiddenStates)
    
    /** Observation symbol emission distribution */
    private val B = ConditionalProbabilityDistribution.uniform(V.size,numHiddenStates)
  
    /**
     * Prior probability of beginning in state i.
     * <p>
     * Note: The state values that this method accepts range from 1 up to and including N
     */
    def π(i:Int) : Double = {
      if (i<1 || i>N) {
        throw new ArrayIndexOutOfBoundsException(i)
      } else {
        // We need to translate 1-based state indices into 0-based state indices
    	return Π(i-1)
      }
    }
    
    /**
     * Probability of emitting observation symbol o
     * from hidden state i.
     * <p>
     * Note: The state values that this method accepts range from 1 up to and including N
     */
    def b(i:Int, o:String) : Double = {
      if (i<1 || i>N) {
        throw new ArrayIndexOutOfBoundsException(i)
      } else {
        // We need to translate 1-based state indices into 0-based state indices
    	return B(V.getInt(o),i-1)
      }
    }
    
    /**
     * Probability of transitioning from hidden state i to hidden state j.
     * <p>
     * Note: The state values that this method accepts range from 1 up to and including N
     */
    def a(i:Int, j:Int) : Double = {
      if (i<1 || i>N) {
        throw new ArrayIndexOutOfBoundsException(i)
      } else if (j<1 || j>N) {
        throw new ArrayIndexOutOfBoundsException(j)
      } else {
        // We need to translate 1-based state indices into 0-based state indices
        return A(j-1,i-1)
      }
    }
    
    val N = numHiddenStates
    
}
