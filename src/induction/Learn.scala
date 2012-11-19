package induction

object Learn {

  def main(args: Array[String]): Unit = {
    
    val d = induction.mutable.ConditionalProbabilityDistribution.uniform(10,3)
    
    println(d)
  }

}