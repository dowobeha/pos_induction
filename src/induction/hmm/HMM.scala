package induction.hmm

import induction.util.OneBasedArray
import scala.collection.mutable.HashMap

class HMM(sentence:String, val λ:ModelParameters) {

  import λ._
  
  /** 
   * Observation sequence
   * <p>
   * Note: This array is 1-based
   */
  val O: OneBasedArray[String] = new OneBasedArray[String](sentence.trim.split("""\s+"""))
  
  /** Number of observations */
  val T: Int = O.length
  
  
  /** Forward probability cache */
  private val previouslyCalculated_α = new HashMap[(Int,Int),Double]
  
  /** Backward probability cache */
  private val previouslyCalculated_β = new HashMap[(Int,Int),Double]

  private val previouslyCalculated_ξ = new HashMap[(Int,Int,Int),Double]
  
  private val previouslyCalculated_γ = new HashMap[(Int,Int),Double]
  
  /** Forward probability of entire observation sequence. */
  def α : Double = {
    var sum = 0.0
    for (i <- 1 to N) {
    	sum += α(T,i)
    }
    return sum
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
		    } else if (j < 1 || j > N) {
		      throw new ArrayIndexOutOfBoundsException(j)
		    } else if (t==1) {
		      //Console.err.println("Base case:      calculating α for t==" + t + ", j==" + j)
		      π(j) * b(j,O(t))
		    } else {
		      //Console.err.println("Recursive case: calculating α for t==" + t + ", j==" + j)
		      var sum = 0.0
		      for (i <- 1 to N) {
		        sum += α(t-1,i) * a(i,j)
		      }
		      sum * b(j,O(t)) 
		    }
    	
    	previouslyCalculated_α.update((t,j),value)
    	
    	return value
    }
  }

  /** Backward probability of entire observation sequence. */
  def β : Double = {
    
    var sum = 0.0
    for (i <- 1 to N) {
    	sum += β(1,i)
    }
    return sum
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
		    } else if (j < 1 || j > N) {
		      throw new ArrayIndexOutOfBoundsException(j)
		    } else if (t==T) {
		      1.0
		    } else {
		      var sum = 0.0
		      for (i <- 1 to N) {
		        sum += a(i,j) * b(j,O(t+1)) * β(t+1,j)
		      }
		      sum
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
    	var sum = 0.0
    	for (j <- 1 to N) {
    		sum += ξ(t,i,j)
    	}
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
    var sum = 0.0
	for (t <- 1 to T) {
		sum += ξ(t,i,j)
	}
	return sum
  }

  /**
   * Returns the count of how often this HMM
   * is expected to transition from state <code>i</code>
   * into any next state.
   */   
  def expectedTransitionsFrom_i(i:Int) : Double = {
    var sum = 0.0
	for (t <- 1 to T) {
		sum += γ(t,i)
	}
	return sum
  }

  /**
   * Returns the count of how often this HMM
   * is expected to emit observation <code>k</code>
   * from state <code>i</code>.
   */   
  def expectedObservationsOf_k_from_i(i:Int, k:Int) : Double = {
    var sum = 0.0
	for (t <- 1 to T) {
		if (k == V.getInt(O(t))) {
			sum += γ(t,i)
		}
	}
	return sum
  }
  
}
