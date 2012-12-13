package grid

import com.thoughtworks.xstream.XStream
import org.jppf.client.JPPFClient;
import org.jppf.client.JPPFJob;
import org.jppf.server.protocol.JPPFTask
import scala.collection.JavaConversions

object JPPF {

    val client = new JPPFClient

    Runtime.getRuntime().addShutdownHook(new Thread(){
    	override def run() {
    		if (! (client == null || client.isClosed())) {
    			client.close();
    		}
    	}
    });
    
    def close {
    	if (! (client == null || client.isClosed())) {
    		client.close();
    	}        
    }
 
    def runTasks[ResultType](jobName:String, tasks:Iterable[JPPFTask]) : Iterable[ResultType] = {
        
        try {
            
        	val job = new JPPFJob
        	job.setName(jobName)
        	for (task <- tasks) {
        		job.addTask(task)
        	}
        	job.setBlocking(true)

        	val completedTasks = JavaConversions.asScalaBuffer(client.submit(job))

        	val exceptions = 
        	completedTasks.collect{
        		case task:JPPFTask if task.getException!=null 
        			=> task.getException 
        	}

        	if (! exceptions.isEmpty) {
        		for (exception <- exceptions) {
        			exception.printStackTrace
        		}
        		JPPF.close
        		throw new RuntimeException("Job \"" + jobName + "\" encountered an exception")
        	}

        	val serializedResults = 
        	    completedTasks.map(task => task.getResult.asInstanceOf[String])
        	    
//        	for (s <- serializedResults) {
//        	    System.err.println("Result was " + s.getBytes.length + " bytes, " + s.length + " characters, and " + (s.replaceAll("""[^\n]""","").length+1) + " lines long")
//        	}
        	
        	val deserializedResults : Iterable[ResultType] = 
        		serializedResults.map(serializedResult => grid.JPPF.deserialize[ResultType](serializedResult)) 

        	return deserializedResults
        	
        } catch {
            case exception => {
                JPPF.close
                throw exception
            }
        }
    	
    }
    
    
    
	def serialize[T](t:T) : String = {
        val xstream = new XStream()
        return xstream.toXML(t)
    }
    
    def deserialize[T](serializedObject:String) : T = {
        val xstream = new XStream()
        return xstream.fromXML(serializedObject).asInstanceOf[T]
    }    
    
}
