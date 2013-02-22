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

public class BedlamBundleInit {
    public static void main(String[] args) throws Exception {
	Interpreter i = getBedlamInterpreter(null);
	System.err.println(i.eval("(+ 3 4 5)"));
    }

    public static Interpreter getBedlamInterpreter(String iasylumBedlamLocationParameter) {
	String iasylumBedlamLocation = (iasylumBedlamLocationParameter != null) ? iasylumBedlamLocationParameter : "/home/igorhvr/idm/bedlam/";

	try {
	    AppContext context = new AppContext();
	    
	    context.addDefaultHeap();
	    
	    
	    final Interpreter i = new Interpreter(new ThreadContext(), new DynamicEnvironment(context, System.in, System.out));
	    i.eval("(define bean (make-parameter #f))");
	    i.eval("(define (set-bean v) (bean v) )");
	    Closure setBean = (Closure) retrieveSymbolValue(i, "set-bean");
	    Value[] parameters = new Value[] {new JavaObject(iasylumBedlamLocation)};
	    i.eval(setBean, parameters);
	    i.eval("(import s2j) (define iasylum-bedlam-location (->string (bean)))");
	    i.eval("(begin (load  (string-append iasylum-bedlam-location \"/iasylum/init.scm\"))) ");
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
}
