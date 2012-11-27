package induction.hmm

import induction.util.OneBasedArray
import induction.util.Math
import scala.collection.mutable.HashMap

class HMM(val sentence:String, val λ:ModelParameters) {

  import λ._
  
  /** 
   * Observation sequence
   * <p>
   * Note: This array is 1-based
   */
  val O: OneBasedArray[String] = new OneBasedArray[String](sentence.trim.split("""\s+"""))
  
  /** Number of observations */
  val T: Int = O.length
    
  /** Defines a summation over hidden state indices. */
  def Σ_hiddenStates(f:(Int => Double)) = Math.summation(λ.hiddenStateIndices, f)//λ.hiddenStateIndices.foldLeft(0.0) ((runningSum,i) => runningSum + f(i))
    
  /** Defines a summation over time. */
  def Σ_time(f:(Int => Double)) = Math.summation(1 to T, f)
  
  /** Forward probability cache */
  private val previouslyCalculated_α = new HashMap[(Int,Int),Double]
  
  /** Backward probability cache */
  private val previouslyCalculated_β = new HashMap[(Int,Int),Double]

  private val previouslyCalculated_ξ = new HashMap[(Int,Int,Int),Double]
  
  private val previouslyCalculated_γ = new HashMap[(Int,Int),Double]
  
  /** Forward probability of entire observation sequence. */
  def α : Double = {
    return Σ_hiddenStates( i => α(T,i) )
  }

  /** 
   * Forward probability of observation sequence up to time <code>t</code>.
   * 
   * @param t Word index (1-based index)
   * @param j Hidden state value (1-based index)
   */
  def α(t:Int,j:Int) : Double = {
    
    if (previouslyCalculated_α.contains(t,j)) {

    	// If we have previously calculated α(t,j) just look it up
    	return previouslyCalculated_α(t,j)
    	
    } else {
      
    	// Otherwise calculate α(t,j)	   
    	val value = 
		    if (t < 1 || t > T) {
		      throw new ArrayIndexOutOfBoundsException(t)
		    } else if (j < 0 || j >= N) {
		      throw new ArrayIndexOutOfBoundsException(j)
		    } else if (t==1) {
		      //Console.err.println("Base case:      calculating α for t==" + t + ", j==" + j)
		      π(j) * b(j,O(t))
		    } else {
		      //Console.err.println("Recursive case: calculating α for t==" + t + ", j==" + j)
		      Σ_hiddenStates( i => α(t-1,i) * a(i,j) ) * b(j,O(t))
		    }
    	
    	previouslyCalculated_α.update((t,j),value)
    	
    	return value
    }
  }

  /** Backward probability of entire observation sequence. */
  def β : Double = {
    return Σ_hiddenStates( i => β(1,i) )
//    var sum = 0.0
//    for (i <- 1 to N) {
//    	sum += β(1,i)
//    }
//    return sum
  }
  
  /**
   * Backward probability of the observation sequence after time <code>t</code>.
   * 
   * @param t Word index (1-based index)
   * @param j Hidden state value (1-based index)
   */
  def β(t:Int,j:Int) : Double = {
     
    if (previouslyCalculated_β.contains(t,j)) {

    	// If we have previously calculated β(t,j) just look it up
    	return previouslyCalculated_β(t,j)
    	
    } else {
      
    	// Otherwise calculate β(t,j)	   
    	val value = 
		    if (t < 1 || t > T) {
		      throw new ArrayIndexOutOfBoundsException(t)
		    } else if (j < 0 || j >= N) {
		      throw new ArrayIndexOutOfBoundsException(j)
		    } else if (t==T) {
		      1.0
		    } else {
		      Σ_hiddenStates( i => a(i,j) * b(j,O(t+1)) * β(t+1,j) )
		    }
    	
    	previouslyCalculated_β.update((t,j),value)
    	
    	return value
    }
    
  }
  
  /**
   * Probability of being in state <code>i</code> at time <code>t</code>
   * and transitioning into state <code>j</code> at time <code>t+1</code> 
   */
  def ξ(t:Int, i:Int,j:Int) : Double = {
    if (previouslyCalculated_ξ.contains(t,i,j)) {
      return previouslyCalculated_ξ(t,i,j)
    } else {
      val value = (α(t,i) * a(i,j) * b(j,O(t+1)) * β(t+1,j)) / α 
      previouslyCalculated_ξ.update((t,i,j), value)
      return value
    } 
  }
  
  /**
   * Probability of being in state <code>i</code> at time <code>t</code> 
   */  
  def γ(t:Int, i:Int) : Double = {
    if (previouslyCalculated_γ.contains(t,i)) {
      return previouslyCalculated_γ(t,i)
    } else {
        val sum = 
        	if ( t < T )
        		Σ_hiddenStates( j => ξ(t,i,j) )
        	else
        		α(t,i) / α //TODO Is this line correct?
    	previouslyCalculated_γ.update((t,i),sum)
    	return sum
    }
  }

  
  
  
  /**
   * Returns the count of how often this HMM
   * is expected to begin in state <code>i</code>.
   */
  def expectedStartsFrom_i(i:Int) : Double = {
    return γ(1,i)
  }

  /**
   * Returns the count of how often this HMM
   * is expected to transition from state <code>i</code>
   * into state <code>j</code>.
   */  
  def expectedTransitionsFrom_i_to_j(i:Int, j:Int) : Double = {
    return Σ_time( t => ξ(t,i,j) )
//    var sum = 0.0
//	for (t <- 1 to T) {
//		sum += ξ(t,i,j)
//	}
//	return sum
  }

  /**
   * Returns the count of how often this HMM
   * is expected to transition from state <code>i</code>
   * into any next state.
   */   
  def expectedTransitionsFrom_i(i:Int) : Double = {
      return Σ_time( t => γ(t,i) )
//    var sum = 0.0
//	for (t <- 1 to T) {
//		sum += γ(t,i)
//	}
//	return sum
  }

  /**
   * Returns the count of how often this HMM
   * is expected to emit observation <code>k</code>
   * from state <code>i</code>.
   */   
  def expectedObservationsOf_k_from_i(i:Int, k:Int) : Double = {
      return Σ_time( t => {
          if (k == V.getInt(O(t))) 
              γ(t,i) 
          else 
              0.0 
      })
//    var sum = 0.0
//	for (t <- 1 to T) {
//		if (k == V.getInt(O(t))) {
//			sum += γ(t,i)
//		}
//	}
//	return sum
  }
  
}
