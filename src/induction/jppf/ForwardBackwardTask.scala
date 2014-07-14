package induction.jppf

import grid.JPPF.deserialize
import grid.JPPF.serialize
import induction.hmm.HMM
import induction.hmm.ModelParameters
import induction.hmm.ModelReestimationCounts
import org.jppf.server.protocol.JPPFTask


//class ForwardBackwardTask(hmm:HMM) extends JPPFTask {
class ForwardBackwardTask(private val sentence:String, private val serializedλ:Array[Byte]) extends JPPFTask {
    
    //private val serializedHMM = serialize(hmm)
    
	def run(): Unit = {
        System.out.println("Running task")
	    //val hmm:HMM = deserialize(serializedHMM)
        val λ:ModelParameters = deserialize(serializedλ);
        val hmm = new HMM(sentence,λ)
	    val counts = new ModelReestimationCounts(hmm)
        System.out.println("Done running task")
        System.out.flush
		setResult(serialize(counts))
	}

}
