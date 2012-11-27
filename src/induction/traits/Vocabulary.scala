package induction.traits

import scala.collection.Map
import scala.collection.IndexedSeq

trait Vocabulary {

	val OUT_OF_VOCABULARY : String = null
  
	val stringToInt : Map[String,Int]
	val intToString : IndexedSeq[String]
	
	def getString(i:Int) : String = {
	  return intToString(i)
	}
	
	def inVocabularySize : Int = {
	  return size - 1
	}
	
	def size : Int = {
	  return intToString.size
	}

    /** 
     * Range of vocabulary integer values, 
     * from 0 up to but not including <code>size</code>
     */
    def indices : Range = 0 until size
	
	def getInt(s:String) : Int = {
	  stringToInt.get(s) match {
	    case Some(i) => i
	    case None    => 0
	  }
	}	
	
}