package sisc.contrib.stl;

import javax.servlet.jsp.PageContext;
import sisc.interpreter.Interpreter;

public abstract class TagUtils {

    public static final Interpreter getInterpreter(PageContext pc) {
        return (Interpreter)pc.getAttribute("scheme-context");
    } 
}

