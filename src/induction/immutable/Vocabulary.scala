package induction.immutable

class Vocabulary(v:induction.mutable.Vocabulary) extends induction.traits.Vocabulary {

	val stringToInt = v.stringToInt.toMap
	val intToString = v.intToString
	
}