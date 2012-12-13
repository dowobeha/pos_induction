package induction.jppf

import grid.JPPF.deserialize
import grid.JPPF.serialize
import induction.hmm.HMM
import induction.hmm.ModelReestimationCounts
import org.jppf.server.protocol.JPPFTask


class ForwardBackwardTask(hmm:HMM) extends JPPFTask {
    
    private val serializedHMM = serialize(hmm)
    
	def run(): Unit = { 
	    val hmm:HMM = deserialize(serializedHMM)
	    val counts = new ModelReestimationCounts(hmm)
		setResult(serialize(counts))
	}

}
