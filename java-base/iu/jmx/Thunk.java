package iu.jmx;

import sisc.data.Values;

public class Thunk implements ThunkMBean, java.io.Serializable {
    sisc.data.Closure closure;
    
    public Thunk(sisc.data.Closure closure) {
	this.closure = closure;
    }

    public String execute() {
	sisc.interpreter.Interpreter current = sisc.interpreter.Context.currentInterpreter();
	if(current==null) {
	    current =
		sisc.interpreter.Context.enter(sisc.interpreter.Context.getDefaultAppContext());	    
	}

	try {
	    return ((sisc.data.SchemeString)current.eval(closure,new sisc.data.Value[0])).asString();
	}catch(Exception e) {
	    throw new RuntimeException(e);
	}
    }
}