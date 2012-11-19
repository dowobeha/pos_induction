package induction.util

/**
 * Array where the first element has index 1, 
 * and the final element has index <code>length</code>.
 */
class OneBasedArray[T : ClassManifest](val zeroBasedArray:Array[T]) {
  
	def apply(i: Int) : T = {
	  return zeroBasedArray.apply(i - 1)
	}

	def update(i: Int, x: T) : Unit = {
	  return zeroBasedArray.update(i-1, x)
	}
	
	val length:Int = zeroBasedArray.length	
	
}

