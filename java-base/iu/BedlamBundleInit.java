package iu;

import java.io.IOException;
import java.net.InetAddress;
import java.net.ServerSocket;
import java.util.Date;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import org.apache.log4j.Logger;

import sisc.REPL;
import sisc.data.Closure;
import sisc.data.SchemeVoid;
import sisc.data.Value;
import sisc.data.Values;
import sisc.env.DynamicEnvironment;
import sisc.interpreter.AppContext;
import sisc.interpreter.Interpreter;
import sisc.interpreter.SchemeException;
import sisc.interpreter.ThreadContext;
import sisc.modules.s2j.JavaObject;

import java.io.IOException;
import java.net.URL;
import java.net.URLConnection;
import java.net.URLStreamHandler;

import java.net.URLStreamHandler;
import java.net.URLStreamHandlerFactory;
import java.util.Map;

public class BedlamBundleInit {
    public static void setupClasspathURLHandler() {
	URL.setURLStreamHandlerFactory(new BedlamBundleInit.ConfigurableStreamHandlerFactory("classpath", new BedlamBundleInit.Handler(ClassLoader.getSystemClassLoader())));
    }

    public static void main(String[] args) throws Exception {
	Interpreter i = getBedlamInterpreter(null);
	System.err.println(i.eval("(+ 3 4 5)"));
    }

    public static Interpreter getBedlamInterpreter(String iasylumBedlamLocation) {
	if(iasylumBedlamLocation == null) setupClasspathURLHandler();

	try {
	    AppContext context = new AppContext();
	    
	    context.addDefaultHeap();
	    
	    
	    final Interpreter i = new Interpreter(new ThreadContext(), new DynamicEnvironment(context, System.in, System.out));

	    if(iasylumBedlamLocation != null) {
		i.eval("(define bean (make-parameter #f))");
		i.eval("(define (set-bean v) (bean v) )");
		Closure setBean = (Closure) retrieveSymbolValue(i, "set-bean");
		Value[] parameters = new Value[] {new JavaObject(iasylumBedlamLocation)};
		i.eval(setBean, parameters);
		i.eval("(import s2j) (define iasylum-bedlam-location (->string (bean)))");
		i.eval("(begin (load  (string-append iasylum-bedlam-location \"/iasylum/init.scm\"))) ");
	    } else {
		i.eval("(begin (load  \"classpath:iasylum/init.scm\"))");
	    }

	    return i;
	} catch(Exception e) {
	    throw new RuntimeException(e);
	}
    }

    public static Value retrieveSymbolValue(Interpreter r, String symbolName) {
	try{
	    return r.getContextEnv(r.getSymbol("*toplevel*")).lookup(r.getSymbol(symbolName));
	} catch(Exception e) {
	    throw new RuntimeException(e);
	}
    }

    /** A {@link URLStreamHandler} that handles resources on the classpath. */
    public static class Handler extends URLStreamHandler {
	/** The classloader to find resources from. */
	private final ClassLoader classLoader;
	
	public Handler() {
	    this.classLoader = getClass().getClassLoader();
	}
	
	public Handler(ClassLoader classLoader) {
	    this.classLoader = classLoader;
	}
	
	protected URLConnection openConnection(URL u) throws IOException {
	    final URL resourceUrl = classLoader.getResource(u.getPath());
	    if(resourceUrl == null) throw new IOException("classLoader.getResource(u.getPath()) returned NULL for "+u+" - impossible to open a connection to this resource.");
	    return resourceUrl.openConnection();
	}
    }
    
    public static class ConfigurableStreamHandlerFactory implements URLStreamHandlerFactory {
	private final Map<String, URLStreamHandler> protocolHandlers=new java.util.concurrent.ConcurrentHashMap();
	
	public ConfigurableStreamHandlerFactory(String protocol, URLStreamHandler urlHandler) {
	    addHandler(protocol, urlHandler);
	}
	
	public void addHandler(String protocol, URLStreamHandler urlHandler) {
	    protocolHandlers.put(protocol, urlHandler);
	}
	
	public URLStreamHandler createURLStreamHandler(String protocol) {
	    return protocolHandlers.get(protocol);
	}
    }
    
}
