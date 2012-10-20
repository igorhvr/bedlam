package sisc.contrib.servlets;

import javax.servlet.http.*;
import javax.servlet.*;
import java.io.*;

import sisc.*;
import sisc.data.*;
import sisc.interpreter.*;

public class SchemeServlet extends SchemeServletBase {

    public Procedure getFn, postFn, putFn, deleteFn;

    public void init()
        throws ServletException {
        super.init();
        String initExpr = getInitParameter("init-expr");
        evalExpr(initExpr);
    }

    public void destroy() {
        String destroyExpr = getInitParameter("destroy-expr");
        try {
            evalExpr(destroyExpr);
        } catch (ServletException e) {
            throw new RuntimeException(e.toString());
        }
    }

    private void callFn(Procedure fn,
                        HttpServletRequest request,
                        HttpServletResponse response)
        throws ServletException {

        Interpreter r = Context.enter();
        try {
            r.eval(fn, new Value[] {
                sisc.modules.s2j.Util.makeJObj(request, HttpServletRequest.class),
                    sisc.modules.s2j.Util.makeJObj(response, HttpServletResponse.class) });
        } catch (SchemeException e) {
            throw new ServletException("calling " + fn + " failed", sisc.modules.s2j.Util.javaException(e));
        } finally {
            Context.exit();
        }
    }

    public void doGet(HttpServletRequest request,
                      HttpServletResponse response)
        throws IOException, ServletException {

        if (null == getFn)
            super.doGet(request, response);
        else
            callFn(getFn, request,response);
    }

    public void doPost(HttpServletRequest request,
                       HttpServletResponse response)
        throws IOException, ServletException {

        if (null == postFn)
            super.doPost(request, response);
        else
            callFn(postFn, request,response);
    }
    
    public void doPut(HttpServletRequest request,
                      HttpServletResponse response)
        throws IOException, ServletException {

        if (null == putFn)
            super.doPut(request, response);
        else
            callFn(putFn, request,response);
    }
    
    public void doDelete(HttpServletRequest request,
                         HttpServletResponse response)
        throws IOException, ServletException {

        if (null == deleteFn)
            super.doDelete(request, response);
        else
            callFn(deleteFn, request,response);
    }
    
}


/*
 * The contents of this file are subject to the Mozilla Public
 * License Version 1.1 (the "License"); you may not use this file
 * except in compliance with the License. You may obtain a copy of
 * the License at http://www.mozilla.org/MPL/
 * 
 * Software distributed under the License is distributed on an "AS
 * IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 * implied. See the License for the specific language governing
 * rights and limitations under the License.
 * 
 * The Original Code is SISC Servlets.
 * 
 * The Initial Developer of the Original Code is Matthias Radestock.
 * Portions created by Matthias Radestock are Copyright (C) 2000-2002
 * Matthias Radestock.  All Rights Reserved.
 * 
 * Contributor(s):
 * 
 * Alternatively, the contents of this file may be used under the
 * terms of the GNU General Public License Version 2 or later (the
 * "GPL"), in which case the provisions of the GPL are applicable 
 * instead of those above.  If you wish to allow use of your 
 * version of this file only under the terms of the GPL and not to
 * allow others to use your version of this file under the MPL,
 * indicate your decision by deleting the provisions above and
 * replace them with the notice and other provisions required by
 * the GPL.  If you do not delete the provisions above, a recipient
 * may use your version of this file under either the MPL or the
 * GPL.
 */
