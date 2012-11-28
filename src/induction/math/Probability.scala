package induction.math

import java.lang.Math.E
import java.lang.Math.log10
import java.math.BigDecimal
import java.math.MathContext
import java.math.RoundingMode

class Probability private (private val value : BigDecimal) {

    def this(mathContext:MathContext, i:Int) = {
        this(new BigDecimal(i,mathContext))
    }
    
    def this(i:Int) = {
        this(new BigDecimal(i,Probability.mathContext))
    }

    def this(numerator:BigDecimal, denominator:BigDecimal) = {
        this(numerator.divide(denominator,Probability.mathContext))
    }    
    
    def this(numerator:Int, denominator:Int) = {
        this(new BigDecimal(numerator,Probability.mathContext).divide(new BigDecimal(denominator,Probability.mathContext),Probability.mathContext))
    }

    def this(mathContext:MathContext, numerator:Int, denominator:Int) = {
        this(new BigDecimal(numerator,mathContext).divide(new BigDecimal(denominator,mathContext),mathContext))
    }
    
    def +(p:Probability) = new Probability(value.add(p.value,Probability.mathContext))
    
    def -(p:Probability) = new Probability(value.subtract(p.value,Probability.mathContext))
    
    def *(p:Probability) = new Probability(value.multiply(p.value,Probability.mathContext))
    
    def /(p:Probability) = new Probability(value.divide(p.value,Probability.mathContext))
    
    def logProb : Double = {
        val normalized = value.stripTrailingZeros
        val unscaledValue = normalized.unscaledValue.doubleValue
        val scale = normalized.scale
        val logprob_base10 = log10(unscaledValue) - scale
        val logprob_base_e = logprob_base10 / log10(E) 
        return logprob_base_e
    }
    
    def toBigDecimal = value
    
    override def toString = value.toString
    
}

object Probability {
    private val roundingMode = RoundingMode.HALF_EVEN
    private val precision = 5
    val mathContext = new MathContext(precision,roundingMode)
    val ZERO = new Probability(mathContext,0)
}