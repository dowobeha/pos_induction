package induction.hmm

import induction.mutable.ConditionalProbabilityDistribution
import induction.mutable.PriorProbabilityDistribution
import induction.mutable.Vocabulary

object ModelParameters {
  def uniform(V:Vocabulary, numHiddenStates:Int) : ModelParameters = {
    val m = new ModelParameters(V,numHiddenStates)
    m.Π.setUniform
    m.A.setUniform
    m.B.setUniform
    return m
  }
}

/**
 * Parameters for a Hidden Markov Model, following the terminology of Rabiner & Juang (1986).
 * 
 * @see Rabiner, L.R. and Juang, B.H. 
 *      An Introduction to Hidden Markov Models. 
 *      IEEE ASSP Magazine. January 1986. 
 */
class ModelParameters(val V:Vocabulary, numHiddenStates:Int) {

  	/** Prior distribution over hidden states */
    private val Π = new PriorProbabilityDistribution(numHiddenStates)
    
    /** Hidden state transition distribution */
    private val A = new ConditionalProbabilityDistribution(numHiddenStates,numHiddenStates)
    
    /** Observation symbol emission distribution */
    private val B = new ConditionalProbabilityDistribution(V.size,numHiddenStates)

    def this(V:Vocabulary, numHiddenStates:Int, hmms:Iterable[HMM]) = {
      this(V,numHiddenStates)
      
      //TODO Use expected* methods from HMMs to calculate new values for Π, A, B
      
    }
    
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
