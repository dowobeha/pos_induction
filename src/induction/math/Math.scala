package induction.math

object Math {

	/** Defines a generic mathematical summation function over a sequence. */
	def summation[X](s:Seq[X],f:(X => BigDecimal)) = s.foldLeft(BigDecimal(0)) ((runningSum,i) => runningSum + f(i))

	/** Defines a generic mathematical product function over a sequence. */
	def product[X](s:Seq[X],f:(X => BigDecimal)) = s.foldLeft(BigDecimal(1)) ((runningSum,i) => runningSum * f(i))
	
}
