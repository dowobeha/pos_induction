package induction.hmm

import induction.immutable.ConditionalProbabilityDistribution
import induction.immutable.PriorProbabilityDistribution
import induction.immutable.Vocabulary
import induction.math.Probability
import scala.collection.mutable.HashMap

/**
 * Parameters for a Hidden Markov Model, 
 * following the terminology of Rabiner & Juang (1986).
 * 
 * @see Rabiner, L.R. and Juang, B.H. 
 *      An Introduction to Hidden Markov Models. 
 *      IEEE ASSP Magazine. January 1986. 
 */
class ModelParameters(
        /** Number of hidden states */
        val N: Int,
        /** Prior distribution over hidden states */
        val Π: PriorProbabilityDistribution, 
        /** Hidden state transition distribution */
        val A: ConditionalProbabilityDistribution, 
        /** Observation symbol emission distribution */
        val B: ConditionalProbabilityDistribution, 
        /** Vocabulary of observation symbols */
        val V: Vocabulary
) {
    
    /** 
     * Range of hidden state values, from 0 up to but not including N 
     */
    val hiddenStateIndices : Range = 0 until N
    
    /**
     * Prior probability of beginning in state i.
     * <p>
     * Note: The state values that this method accepts range from 0 up to but not including N
     */
    def π(i:Int) : Probability = {
    	if (i<0 || i>=N) {
    		throw new ArrayIndexOutOfBoundsException(i)
    	} else {
    		return Π(i)
    	}
    }
    
    /**
     * Probability of emitting observation symbol o
     * from hidden state i.
     * <p>
     * Note: The state values that this method accepts range from 0 up to but not including N
     */
    def b(i:Int, o:String) : Probability = {
      	if (i<0 || i>=N) {
      		throw new ArrayIndexOutOfBoundsException(i)
      	} else {
      		return B(i, V.getInt(o))
      	}
    }
    
    /**
     * Probability of transitioning from hidden state i to hidden state j.
     * <p>
     * Note: The state values that this method accepts range from 0 up to but not including N
     */
    def a(i:Int, j:Int) : Probability = {
		if (i<0 || i>=N) {
			throw new ArrayIndexOutOfBoundsException(i)
		} else if (j<0 || j>=N) {
			throw new ArrayIndexOutOfBoundsException(j)
		} else {
			return A(i,j)
		}
    }
       
}

/**
 * Companion object defines helper functions 
 * for the initialization and re-estimation of 
 * parameters for a Hidden Markov Model, 
 * following the terminology of Rabiner & Juang (1986).
 * 
 * @see Rabiner, L.R. and Juang, B.H. 
 *      An Introduction to Hidden Markov Models. 
 *      IEEE ASSP Magazine. January 1986. 
 */
object ModelParameters {
	
    private def bigDecimalZero = BigDecimal(0,Probability.mathContext)
    
    /** Initialize a uniform set of model parameters */
	def uniform(V:Vocabulary, numHiddenStates:Int) : ModelParameters = {

		import induction.mutable.ConditionalProbabilityDistribution
		import induction.mutable.PriorProbabilityDistribution
	    
    	val Π = PriorProbabilityDistribution.uniform(numHiddenStates).immutable
    	val A = ConditionalProbabilityDistribution.uniform(numHiddenStates,numHiddenStates).immutable
    	val B = ConditionalProbabilityDistribution.uniform(V.size,numHiddenStates).immutable
	  
    	return new ModelParameters(numHiddenStates, Π, A, B, V)
    }

	def reestimate(λ:ModelParameters, hmms:Iterable[HMM]) : ModelParameters = {
	    
	    System.err.println("Re-estimating Π")
	    val Π = reestimate_Π(λ.N, hmms)
	    System.err.println("Re-estimating A")
	    val A = reestimate_A(λ.N, hmms)
	    System.err.println("Re-estimating B")
	    val B = reestimate_B(λ.V, λ.N, hmms)
	    
	    return new ModelParameters(λ.N, Π, A, B, λ.V)
	}
	
    /** 
     * Use <code>HMM.expectedStartsFrom</code> to calculate new values for Π
     */
    private def reestimate_Π(numHiddenStates:Int, hmms:Iterable[HMM]) : PriorProbabilityDistribution = {
    	import induction.mutable.PriorProbabilityDistribution
    	
        val expectedStartsFrom = new HashMap[Int,BigDecimal] {
    	    override def default(key: Int) = bigDecimalZero
    	}
    	
    	var totalExpectedStarts = bigDecimalZero
    	for (hmm <- hmms) {
    	    System.err.print(".")
    		hmm.λ.hiddenStateIndices.foreach(i => {
    			val count  = hmm.expectedStartsFrom_i(i).toBigDecimal
    			expectedStartsFrom(i) += count
    			totalExpectedStarts   += count
    		})
    	}
	    System.err.print("\n")  
    	val Π = new PriorProbabilityDistribution(numHiddenStates)
    	hmms.head.λ.hiddenStateIndices.foreach(i => {
    		Π(i) = new Probability(expectedStartsFrom(i),totalExpectedStarts)
    	})
	      
    	return Π.immutable
    }
    
    /** 
     * Use <code>HMM.expectedTransitionsFrom_i_to_j</code> 
     * and <code>HMM.expectedTransitionsFrom_i</code> 
     * to calculate new values for A
     */    
    private def reestimate_A(numHiddenStates:Int, hmms:Iterable[HMM]) : ConditionalProbabilityDistribution = {

        import induction.mutable.ConditionalProbabilityDistribution
        
        val expectedTransitionsFrom = new HashMap[(Int,Int),BigDecimal] {
    	    override def default(key: (Int,Int)) = bigDecimalZero
    	}
        
        val totalExpectedTransitionsFrom = new HashMap[Int,BigDecimal] {
            override def default(key: Int) = bigDecimalZero
        }
        
        hmms.foreach( hmm => {
            System.err.print(".")
            hmm.λ.hiddenStateIndices.foreach(i => {
                totalExpectedTransitionsFrom(i) += hmm.expectedTransitionsFrom_i(i).toBigDecimal
            	hmm.λ.hiddenStateIndices.foreach(j => {
            		expectedTransitionsFrom(i,j) += hmm.expectedTransitionsFrom_i_to_j(i, j).toBigDecimal
            	})
            })
        })
        System.err.print("\n")
        val A = new ConditionalProbabilityDistribution(numHiddenStates,numHiddenStates)
        
        hmms.head.λ.hiddenStateIndices.foreach(i => {
    		hmms.head.λ.hiddenStateIndices.foreach(j => {
    			A(i,j) = new Probability(expectedTransitionsFrom(i,j),totalExpectedTransitionsFrom(i))
    		})
    	})
        
        return A.immutable        
    }
    
    private def reestimate_B(V:Vocabulary, numHiddenStates:Int, hmms:Iterable[HMM]) : ConditionalProbabilityDistribution = {
        import induction.mutable.ConditionalProbabilityDistribution
        
        val expectedObservations = new HashMap[(Int,Int),BigDecimal] {
            override def default(key: (Int,Int)) = bigDecimalZero
        }
        
        val totalExpectedTransitionsFrom = new HashMap[Int,BigDecimal] {
            override def default(key: Int) = bigDecimalZero
        }

        hmms.foreach( hmm => {
            System.err.print(".")
            hmm.λ.hiddenStateIndices.foreach(i => {
                totalExpectedTransitionsFrom(i) += hmm.expectedTransitionsFrom_i(i).toBigDecimal
            	V.indices.foreach(k => {
            		expectedObservations(i,k) += hmm.expectedObservationsOf_k_from_i(i, k).toBigDecimal
            	})
            })
        })        
        System.err.print("\n")
        val B = new ConditionalProbabilityDistribution(V.size,numHiddenStates)
        hmms.head.λ.hiddenStateIndices.foreach(i => {
    		V.indices.foreach(k => {
    			B(i,k) = new Probability(expectedObservations(i,k),totalExpectedTransitionsFrom(i))
    		})
    	})
    	
        return B.immutable
    }
  
}

