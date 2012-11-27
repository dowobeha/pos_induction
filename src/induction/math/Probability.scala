package induction.math

import java.math.MathContext
import java.math.RoundingMode

class Probability private (private val value : BigDecimal) {

    def this(mathContext:MathContext, i:Int) = {
        this(BigDecimal(i,mathContext))
    }
    
    def this(i:Int) = {
        this(BigDecimal(i,Probability.mathContext))
    }

    def this(numerator:BigDecimal, denominator:BigDecimal) = {
        this(numerator / denominator)
    }    
    
    def this(numerator:Int, denominator:Int) = {
        this(BigDecimal(numerator,Probability.mathContext) / BigDecimal(denominator,Probability.mathContext))
    }

    def this(mathContext:MathContext, numerator:Int, denominator:Int) = {
        this(BigDecimal(numerator,mathContext) / BigDecimal(denominator,mathContext))
    }
    
    def +(p:Probability) = new Probability(value + p.value)
    
    def -(p:Probability) = new Probability(value - p.value)
    
    def *(p:Probability) = new Probability(value * p.value)
    
    def /(p:Probability) = new Probability(value / p.value)
    
    def toBigDecimal = value
    
    override def toString = value.toString
    
}

object Probability {
    private val roundingMode = RoundingMode.HALF_EVEN
    private val precision = 5
    val mathContext = new MathContext(precision,roundingMode)    
}