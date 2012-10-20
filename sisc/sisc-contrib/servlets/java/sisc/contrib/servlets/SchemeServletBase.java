package sisc.contrib.servlets;

import javax.servlet.http.*;
import javax.servlet.*;
import java.io.*;

import sisc.*;
import sisc.data.*;
import sisc.interpreter.*;

import java.util.Hashtable;

import sisc.util.Util;

public class SchemeServletBase extends HttpServlet {

    protected static Hashtable htmlEscapes = new Hashtable();

    static {
        htmlEscapes.put("&", "&amp;");          // &
        htmlEscapes.put("\"", "&quot;");        // "
        htmlEscapes.put(">", "&gt;");           // >
        htmlEscapes.put("<", "&lt;");           // <
        htmlEscapes.put("'", "&#39;");          // '
    }

    public void init()
        throws ServletException {
        String scName = getServletContext().getServletContextName();
    }

    protected static String searchReplace(String in, Hashtable pairs) {
        StringBuffer out = new StringBuffer(in.length()*2);
        String c;
        String r = null;
        int len = in.length();
        for (int i = 0; i < len; i++)
            {
                c = in.substring(i, i+1);
                r = (String) pairs.get(c);
                if (r != null) out.append(r);
                else out.append(c);
            }
        return out.toString();
    }

    public void evalExpr(String expr)
        throws ServletException {

        if (expr == null) return;
        Interpreter r = Context.enter();
        try {
            Procedure currDir = (Procedure)r.lookup(Symbol.get("current-directory"), Util.TOPLEVEL);
            r.eval(currDir, new Value[] {new SchemeString(getServletContext().getRealPath("/"))});
            Procedure p = (Procedure)r.eval(expr);
            r.eval(p, new Value[] { sisc.modules.s2j.Util.makeJObj(this) });
        } catch (IOException e) {
            throw new ServletException("evaluating " + expr + " failed", e);
        } catch (SchemeException e) {
            throw new ServletException("calling " + expr + " failed", e);
        } finally {
            Context.exit();
        }
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
