package sisc.contrib.stl;

import javax.servlet.jsp.tagext.*;
import javax.servlet.jsp.*;
import java.io.IOException;
import sisc.data.Value;
import sisc.util.Util;

public class SchemeTag extends BodyTagSupport {

    public int doAfterBody() throws JspTagException {
        BodyContent bc = getBodyContent();
        Interpreter r=TagUtils.getInterpreter(pageContext);
        String bodyText=bc.getString();
        System.err.println("eval:"+bodyText);
        bc.clearBody();

        try {
            Value v=r.eval(bodyText);
            if (v!=Util.VOID) {
                getPreviousOut().print(v);
            }
        } catch (IOException ie) {
            ie.printStackTrace();
        } catch (SchemeException se) {
            se.printStackTrace();
        }
        
        return EVAL_PAGE;
    }
}
