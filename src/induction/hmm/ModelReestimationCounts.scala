package induction.hmm

import induction.immutable.ConditionalProbabilityDistribution
import induction.immutable.PriorProbabilityDistribution
import induction.immutable.Vocabulary
import induction.math.Probability
import java.math.BigDecimal
import scala.collection.mutable.HashMap

class ModelReestimationCounts private (
        private val numHiddenStates:Int, 
        private val hiddenStateIndices:Range,
        private val V:Vocabulary
    ) {
    
    
    /** Counts needed to reestimate Π */
    private val expectedStartsFrom = new HashMap[Int,BigDecimal] {
    	override def default(key: Int) = ModelParameters.bigDecimalZero
    }
        
    /** Numerator counts needed to reestimate A */
    private val expectedTransitionsFrom = new HashMap[(Int,Int),BigDecimal] {
    	override def default(key: (Int,Int)) = ModelParameters.bigDecimalZero
    }

    /** Denominator counts needed to reestimate A and B */
    private val totalExpectedTransitionsFrom = new HashMap[Int,BigDecimal] {
    	override def default(key: Int) = ModelParameters.bigDecimalZero
    }

    /** Observation counts needed to reestimate B */
    private val expectedObservations = new HashMap[(Int,Int),BigDecimal] {
    	override def default(key: (Int,Int)) = ModelParameters.bigDecimalZero
    }
    
    def this(λ:ModelParameters) = {
        this(λ.N, λ.hiddenStateIndices, λ.V)
    }
    
    def this(hmm:HMM) = {
        this(hmm.λ.N, hmm.λ.hiddenStateIndices, hmm.λ.V)       
        
        // for each hidden state i ...
        hiddenStateIndices.foreach(i => {

            // Calculate the number of transitions that begin in state i at time 1 (used to reestimate Π)
            expectedStartsFrom(i) = expectedStartsFrom(i).add(hmm.expectedStartsFrom_i(i).toBigDecimal)
            
            // Calculate the number of transitions that begin at state i (used to reestimate A and B)
            totalExpectedTransitionsFrom(i) = totalExpectedTransitionsFrom(i).add(hmm.expectedTransitionsFrom_i(i).toBigDecimal)

            // Calculate the number of transitions from state i to state j (used to reestimate A)
            hiddenStateIndices.foreach(j => {
        		expectedTransitionsFrom((i,j)) = expectedTransitionsFrom(i,j).add(hmm.expectedTransitionsFrom_i_to_j(i, j).toBigDecimal)
        	})
        	
        	// Calculate the number of observations of k generated by state i (used to reestimate B)
        	V.indices.foreach(k => {
        		expectedObservations((i,k)) = expectedObservations(i,k).add(hmm.expectedObservationsOf_k_from_i(i, k).toBigDecimal)
        	})        	
        	
        })      
    }
    
    def +(other:ModelReestimationCounts) : ModelReestimationCounts = {
        val summedCounts = new ModelReestimationCounts(numHiddenStates,hiddenStateIndices,V)
        
        hiddenStateIndices.foreach( i => {
        
//            System.err.println("this.expectedStartsFrom("+i+")==null")
//            System.err.println(this.expectedStartsFrom(i)==null)
//            System.err.println("other")
//            System.err.println(other==null)
//            System.err.println("other.expectedStartsFrom("+i+")==null")       
//            System.err.println(other.expectedStartsFrom(i)==null)
//            System.err.println("summedCounts.totalExpectedTransitionsFrom("+i+")==null")
//            System.err.println(summedCounts.totalExpectedTransitionsFrom(i)==null)
            summedCounts.expectedStartsFrom(i) = this.expectedStartsFrom(i).add(other.expectedStartsFrom(i))
        
            summedCounts.totalExpectedTransitionsFrom(i) = this.totalExpectedTransitionsFrom(i).add(other.totalExpectedTransitionsFrom(i))
            
            hiddenStateIndices.foreach( j => {
                summedCounts.expectedTransitionsFrom((i,j)) = this.expectedTransitionsFrom((i,j)).add(other.expectedTransitionsFrom((i,j)))
            })
        
            V.indices.foreach(k => {
            	summedCounts.expectedObservations((i,k)) = this.expectedObservations((i,k)).add(other.expectedObservations((i,k)))
            })
            
        })
        
        return summedCounts
    }

	def reestimateModelParameters : ModelParameters = {
	    
	    System.err.println("Re-estimating Π")
	    val Π = reestimate_Π
	    System.err.println("Re-estimating A")
	    val A = reestimate_A
	    System.err.println("Re-estimating B")
	    val B = reestimate_B
	    
	    return new ModelParameters(numHiddenStates, Π, A, B, V)
	}    
    
    /** 
     * Use <code>HMM.expectedStartsFrom</code> to calculate new values for Π
     */
    private def reestimate_Π : PriorProbabilityDistribution = {
    	import induction.mutable.PriorProbabilityDistribution

    	// Sum up all the values in the expectedStartsFrom map
    	val initialSum = ModelParameters.bigDecimalZero
    	val totalExpectedStarts:BigDecimal = expectedStartsFrom.foldLeft(initialSum) (
    		(runningSum,keyValueTuple) => runningSum.add(keyValueTuple._2)
    	)

    	val Π = new PriorProbabilityDistribution(numHiddenStates)
    	hiddenStateIndices.foreach(i => {
    		Π(i) = new Probability(expectedStartsFrom(i),totalExpectedStarts)
    	})

    	return Π.immutable
    }

    
    /** 
     * Use <code>HMM.expectedTransitionsFrom_i_to_j</code> 
     * and <code>HMM.expectedTransitionsFrom_i</code> 
     * to calculate new values for A
     */    
    private def reestimate_A : ConditionalProbabilityDistribution = {

        import induction.mutable.ConditionalProbabilityDistribution

        val A = new ConditionalProbabilityDistribution(numHiddenStates,numHiddenStates)

        hiddenStateIndices.foreach(i => {
        	hiddenStateIndices.foreach(j => {
        		A(i,j) = new Probability(expectedTransitionsFrom(i,j),totalExpectedTransitionsFrom(i))
        	})
        })
        
        return A.immutable        
    }    

    
    private def reestimate_B : ConditionalProbabilityDistribution = {
        import induction.mutable.ConditionalProbabilityDistribution
        
        val B = new ConditionalProbabilityDistribution(V.size,numHiddenStates)
        
        hiddenStateIndices.foreach(i => {
        	V.indices.foreach(k => {
    			B(i,k) = new Probability(expectedObservations(i,k),totalExpectedTransitionsFrom(i))
    		})
    	})
    	
        return B.immutable
    }
    
}