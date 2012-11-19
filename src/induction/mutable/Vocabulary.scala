package induction.mutable

import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer

class Vocabulary extends induction.traits.Vocabulary {

	val stringToInt = new HashMap[String,Int]
	val intToString = new ArrayBuffer[String]

	intToString.append(OUT_OF_VOCABULARY)
	
	override def getInt(s:String) : Int = {
	  if (! stringToInt.contains(s)) {
	     stringToInt.update(s,this.size)
	     intToString.append(s)
	  }
	  
	  return stringToInt(s)
	}
	
}
