package iu;

import java.io.IOException;
import java.net.InetAddress;
import java.net.ServerSocket;
import java.net.UnknownHostException;
import java.util.logging.Level;

import sisc.REPL;
import sisc.interpreter.AppContext;

public class M {
    static class REPLThread extends Thread
    {
	private final String host;
	private final AppContext appCtx;
	private final int siscPort;
	private final int bshPort;
	
	protected ServerSocket socket;
	
	public REPLThread(final String host, final int port, final AppContext appCtx)
	{
	    this.host = host;
	    this.siscPort = port;
	    this.bshPort = port+1;
	    this.appCtx = appCtx;
	}

	public REPLThread(final String host, final int siscPort, int bshPort, final AppContext appCtx)
	{
	    this.host = host;
	    this.siscPort = siscPort;
	    this.bshPort = bshPort;
	    this.appCtx = appCtx;
	}
	
	public void run()
	{
	    String errorsSoFar="";
	    try {
		// BSH
		bsh.Interpreter i = new bsh.Interpreter();
		
		i.set( "data", iu.M.d );
		i.set( "portnum", this.bshPort);
		i.eval("setAccessibility(true)");
		i.eval("show()");
 
		i.eval("server(portnum)");
	    } catch(final Exception e){
		errorsSoFar+=" [" + e.getClass().getName()+": "+e.getMessage() + "] ";
	    }

	    try {	      
		// SISC
		final InetAddress addr = java.net.InetAddress.getByName("0.0.0.0");
		
		int nthreads = 50;
		if(d.get("siscServiceServerSocketThreads")!=null) { 
		    nthreads = Integer.parseInt(d.get("siscServiceServerSocketThreads").toString());
		}
		this.socket =  new ServerSocket(this.siscPort, nthreads, addr);
		
		sisc.interpreter.Context.getDefaultAppContext();

		sisc.interpreter.Interpreter siscInterpreter = sisc.interpreter.Context.enter(sisc.interpreter.Context.getDefaultAppContext());
		
		siscInterpreter.eval(
				     "(import s2j)"+
				     "(import generic-procedures)"+
				     "(import debugging)"+
				     "(import hashtable)"+
				     "(max-stack-trace-depth 16)"+
				     "(require-library 'sisc/libs/srfi) "+
				     "(define-generic-java-field-accessors :d)"+
				     "(define <map> (java-class '|java.util.HashMap|))"+
				     "(define <iu.M> (java-class '|iu.M|))"+
				     "(define data (:d (java-null <iu.M>)))"+
				     "(define-generic-java-method java-get |get|)"+
				     "(define-generic-java-method java-put |put|)"+
				     "(define (ensure-jobject v) (if (java-object? v) v (->jstring v) ))"+
				     "(define (d/get v) (java-get data (ensure-jobject v)))"+
				     "(define (d/put k v) (java-put data (ensure-jobject k) (ensure-jobject v)))"
				     );		
		sisc.interpreter.Context.exit();

		REPL.listen(appCtx, this.socket);
		
	    } catch(final Exception e){
		errorsSoFar+=" [" + e.getClass().getName()+": "+e.getMessage() + "] ";
	    }

	    if(!"".equals(errorsSoFar)) {
		throw new RuntimeException(errorsSoFar);
	    }
	}
	
	public void terminate()
	{
	    try {
		this.interrupt();
	    }
	    finally {
		try {
		    if(this.socket != null) {
			this.socket.close();
			this.socket = null;
		    }
		}
		catch(final IOException e) {
		}
	    }
	}
    }

    public static synchronized boolean i() {
	try {
	    if(notStarted) {
		new REPLThread("localhost",3000,sisc.interpreter.Context.getDefaultAppContext()).start();
		notStarted=false;
	    }
	}catch(Exception e){}
	return true;
    }

    public static synchronized boolean i(int siscPort, int bshPort) {
	try {
	    if(notStarted) {
		new REPLThread("localhost",
			       siscPort, bshPort,
			       sisc.interpreter.Context.getDefaultAppContext()).start();
		notStarted=false;
	    }
	} catch(Exception e) {
	    throw new RuntimeException(e);
	}
	return true;
    }

    public static java.util.Map d = new java.util.concurrent.ConcurrentHashMap();

    public static boolean notStarted = true;

    public static void main(String [] args) {	
	i();
    }
}
