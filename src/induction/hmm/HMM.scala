package induction.hmm

import induction.util.OneBasedArray
import scala.collection.mutable.HashMap

class HMM(sentence:String, val λ:ModelParameters) {

  /** 
   * Observation sequence
   * <p>
   * Note: This array is 1-based
   */
  val O: OneBasedArray[String] = new OneBasedArray[String](sentence.trim.split("""\s+"""))
  
  /** Number of observations */
  val T: Int = O.length
  
  
  /** Forward probability of entire observation sequence. */
  def α : Double = {
    import λ._
    
    var sum = 0.0
    for (i <- 1 to N) {
    	sum += α(T,i)
    }
    return sum
  }
  
  val αCache = new HashMap[(Int,Int),Double]

  /** 
   * Forward probability of observation sequence up to time <code>t</code>.
   * 
   * @param t Word index (1-based index)
   * @param j Hidden state value (1-based index)
   */
  def α(t:Int,j:Int) : Double = {
    
    if (αCache.contains(t,j)) {

    	// If we have previously calculated α(t,j) just look it up
    	return αCache(t,j)
    	
    } else {
      
    	// Otherwise calculate α(t,j)
      
    	import λ._
	   
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
    	
    	αCache.update((t,j),value)
    	
    	return value
    }
  }

  /** Backward probability of entire observation sequence. */
  def β : Double = {
    import λ._
    
    var sum = 0.0
    for (i <- 1 to N) {
    	sum += β(1,i)
    }
    return sum
  }
  
  val βCache = new HashMap[(Int,Int),Double]
  
  /**
   * Backward probability of the observation sequence after time <code>t</code>.
   * 
   * @param t Word index (1-based index)
   * @param j Hidden state value (1-based index)
   */
  def β(t:Int,j:Int) : Double = {
     
    if (βCache.contains(t,j)) {

    	// If we have previously calculated β(t,j) just look it up
    	return βCache(t,j)
    	
    } else {
      
    	// Otherwise calculate β(t,j)
    	
    	import λ._
	   
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
    	
    	βCache.update((t,j),value)
    	
    	return value
    }
    
  }
  
}
