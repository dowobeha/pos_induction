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

        	System.err.println("Submitting JPPF tasks to be run")
        	System.err.flush
        	val timeBeforeRunningTasks = (new java.util.Date).getTime
        	val completedTasks = JavaConversions.asScalaBuffer(client.submit(job))
        	val timeAfterRunningTasks = (new java.util.Date).getTime
        	System.err.println("JPPF tasks have completed")
        	System.err.flush
        	
        	val exceptions = 
        	completedTasks.collect{
        		case task:JPPFTask if task.getException!=null 
        			=> task.getException 
        	}

        	if (exceptions.isEmpty) {
        	    System.err.println("JPPF tasks completed without throwing an exception")
        	    System.err.flush
        	} else {
        	    System.err.println("JPPF tasks completed, but threw an exception")
        	    System.err.flush
        		for (exception <- exceptions) {
        			exception.printStackTrace
        		}
        		JPPF.close
        		throw new RuntimeException("Job \"" + jobName + "\" encountered an exception")
        	}

        	val serializedResults = 
        	    completedTasks.map(task => task.getResult.asInstanceOf[Array[Byte]])
        	    
//        	for (s <- serializedResults) {
//        	    System.err.println("Result was " + s.getBytes.length + " bytes, " + s.length + " characters, and " + (s.replaceAll("""[^\n]""","").length+1) + " lines long")
//        	}
        	
        	System.err.println("Deserializing results")
        	System.err.flush
        	val timeBeforeDeserializingResults = (new java.util.Date).getTime
        	val deserializedResults = new scala.collection.mutable.ArrayBuffer[ResultType](serializedResults.size)
        	for (serializedResult <- serializedResults) {
        	    val before = (new java.util.Date).getTime
        	    val r = grid.JPPF.deserialize[ResultType](serializedResult)
        	    val beforeAppend = (new java.util.Date).getTime
        	    deserializedResults.append(r)
        	    val after = (new java.util.Date).getTime
        	    val timeDeserialize = (beforeAppend - before) / 1000.0
        	    val timeAppend = (after - beforeAppend) / 1000.0
        	    System.err.println("Deserializing a single result took " + timeDeserialize + " seconds; appending result to buffer took " + timeAppend + " seconds")
        	}
//        	val deserializedResults : Iterable[ResultType] = 
//        		serializedResults.map(serializedResult => grid.JPPF.deserialize[ResultType](serializedResult)) 
        	val timeAfterDeserializingResults = (new java.util.Date).getTime
        	
        	val timeRunTasks = (timeAfterRunningTasks - timeBeforeRunningTasks) / 1000.0
        	val timeDeserialize = (timeAfterDeserializingResults - timeBeforeDeserializingResults) / 1000.0
        	
        	System.err.println("Running tasks took " + timeRunTasks + " seconds; deserializing results took " + timeDeserialize + " seconds")
        	
        	return deserializedResults
        	
        } catch {
            case exception => {
                System.err.println("Encountered exception - closing JPPF")
                JPPF.close
                throw exception
            }
        }
    	
    }
    
    def serializeToString[T](t:T) : String = {
	    val xstream = new XStream()
	    val serializedString = xstream.toXML(t)
	    
	    return serializedString
    }
    
    def deserializeFromString[T](serializedString:String) : T = {
    	val xstream = new XStream()
    	val deserializedObject:T = xstream.fromXML(serializedString).asInstanceOf[T]  
        
    	return deserializedObject
    }
    
	def serializeToBytes[T](t:T) : Array[Byte] = {
	    import java.io.ByteArrayOutputStream
	    import java.util.zip.GZIPOutputStream
	    
	    val timeStart = (new java.util.Date).getTime
	    
	    val serializedString = serializeToString(t)
	    
	    val timeMiddle = (new java.util.Date).getTime
	    
	    val           output = new ByteArrayOutputStream
	    val compressedOutput = new GZIPOutputStream(output)

	    compressedOutput.write(serializedString.getBytes("UTF-8"))
	    compressedOutput.close
	    
	    val serializedCompressedBytes = output.toByteArray
	    output.close

	    val timeEnd = (new java.util.Date).getTime
	    
	    val timeFirstHalf  = (timeMiddle - timeStart) / 1000.0
	    val timeSecondHalf = (timeEnd - timeMiddle) / 1000.0
	    
	    System.err.println("Serializing to string took " + timeFirstHalf + " seconds; compressing to bytes took " + timeSecondHalf + " seconds")
	    
        return serializedCompressedBytes
    }
    
    def deserializeFromBytes[T](serializedCompressedBytes:Array[Byte]) : T = {
	    import java.io.ByteArrayInputStream
	    import java.util.zip.GZIPInputStream
	    import scala.io.Source
	    val timeStart = (new java.util.Date).getTime
	    val   compressedInput = new ByteArrayInputStream(serializedCompressedBytes)
	    val decompressedInput = new GZIPInputStream(compressedInput)
	    
	    val reader = Source.fromInputStream(decompressedInput,"UTF-8")
	    val string = reader.mkString
//	    
//	    System.err.println("  Compressed result was " + serializedCompressedBytes.length + " bytes")
//	    System.err.println("Decompressed result was " + string.getBytes.length + " bytes, " + string.length + " characters, and " + (string.replaceAll("""[^\n]""","").length+1) + " lines long")
	    
	    decompressedInput.close
	    compressedInput.close
	    val timeMiddle = (new java.util.Date).getTime
	    
	    val deserializedObject:T = deserializeFromString(string)
	    
	    val timeEnd = (new java.util.Date).getTime

	    val timeFirstHalf  = (timeMiddle - timeStart) / 1000.0
	    val timeSecondHalf = (timeEnd - timeMiddle) / 1000.0
	    
	    System.err.println("Deserializing from string took " + timeSecondHalf + " seconds; decompressing from bytes took " + timeFirstHalf + " seconds")
	    
        return deserializedObject
    }    
    
        
    def serialize[T](t:T) : Array[Byte] = serializeToBytes(t)
    def deserialize[T](o:Array[Byte]) : T = deserializeFromBytes(o)
    
}
