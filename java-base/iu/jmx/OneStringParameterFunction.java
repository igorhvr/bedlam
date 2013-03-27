package iu.jmx;

import sisc.data.Values;

public class OneStringParameterFunction implements OneStringParameterFunctionMBean, java.io.Serializable {
    sisc.data.Closure closure;
    
    public OneStringParameterFunction(sisc.data.Closure closure) {
	this.closure = closure;
    }

    public String execute(String p) {
	sisc.interpreter.Interpreter current = sisc.interpreter.Context.currentInterpreter();
	if(current==null) {
	    current =
		sisc.interpreter.Context.enter(sisc.interpreter.Context.getDefaultAppContext());
	    
	}

	try {
	    sisc.data.Value[] parameters=new sisc.data.Value[1];
	    parameters[0]=new sisc.data.SchemeString(p);

	    return ((sisc.data.SchemeString)current.eval(closure,parameters)).asString();
	}catch(Exception e) {
	    throw new RuntimeException(e);
	}
    }
}