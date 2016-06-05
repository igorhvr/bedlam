/* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1/GPL 2.0/LGPL 2.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is SISCweb.
 *
 * The Initial Developer of the Original Code is Alessandro Colomba.
 * Portions created by the Alessandro Colomba are Copyright (C) 2005-2007
 * Alessandro Colomba. All Rights Reserved.
 *
 * Contributor(s):
 * Dan Muresan
 *
 * Alternatively, the contents of this file may be used under the terms of
 * either the GNU General Public License Version 2 or later (the "GPL"), or
 * the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
 * in which case the provisions of the GPL or the LGPL are applicable instead
 * of those above. If you wish to allow use of your version of this file only
 * under the terms of either the GPL or the LGPL, and not to allow others to
 * use your version of this file under the terms of the MPL, indicate your
 * decision by deleting the provisions above and replace them with the notice
 * and other provisions required by the GPL or the LGPL. If you do not delete
 * the provisions above, a recipient may use your version of this file under
 * the terms of any one of the MPL, the GPL or the LGPL.
 *
 * ***** END LICENSE BLOCK ***** */

package siscweb.web;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.logging.Level;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.servlet.ServletContext;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import sisc.data.Procedure;
import sisc.data.Symbol;
import sisc.data.Value;
import sisc.interpreter.AppContext;
import sisc.interpreter.Context;
import sisc.interpreter.Interpreter;
import sisc.interpreter.SchemeCaller;
import sisc.interpreter.SchemeException;
import siscweb.contcentric.AppContextLocator;
import siscweb.util.Logger;
import siscweb.util.MiscUtil;


/**
 * This class adapts SISCweb's <code>sisclet</code> function to the
 * J2EE HttpServlet interface. Two parameters, "on-init-sexp" and
 * "on-destroy-sexp" are looked up (from the servlet declaration in
 * web.xml) respectively upon init() and destroy() and evaluated as
 * scheme code in the SISC context. The code in the two on-*-sexp should be used to
 * setup and tear-down resources, publish functions, etc.  More than
 * one SISCAdapterServlet can be used in a given context-name in order
 * to provide multiple base url patterns to scheme code.  Note that
 * the <code>sisclet</code> function should have been setup by
 * specifying an instance of SISCApplicationServlet in the web.xml.
 *
 * @see siscweb.web.SISCApplicationServlet
 *
 */
public class SISCAdapterServlet extends HttpServlet
{
    private static final long serialVersionUID = 7683553386599453481L;

    protected String contextName;
    protected URL contextRoot;

    public void init()
        throws ServletException
    {
        try {
            this.contextRoot = this.getServletContext().getResource("/");

            final String servletName = this.getServletName();

            if(Logger.logger.isLoggable(Level.FINER)) {
                Logger.logger.finer("Servlet " + servletName +
                        " being initialized.");
            }

            final String sexp = this.getInitParameter("on-init-sexp");

            if(Logger.logger.isLoggable(Level.INFO)) {
                Logger.logger.info("Evaluating on-init-sexp for servlet : " + servletName);
            }

            this.eval(sexp);
        }
        catch(final MalformedURLException mue) {
            if(Logger.logger.isLoggable(Level.SEVERE)) {
                Logger.logger.severe(mue.getMessage());
            }

            throw new ServletException(mue);
        }
        catch(final SchemeException se) {
            if(Logger.logger.isLoggable(Level.SEVERE)) {
                Logger.logger.severe(se.getMessage());
            }

            throw new ServletException(se);
        }
    }

    public void destroy()
    {
        try {
            this.contextRoot = null;

            final String servletName = this.getServletName();

            if(Logger.logger.isLoggable(Level.FINER)) {
                Logger.logger.finer("Servlet " + servletName +
                        " being destroyed.");
            }

            final String sexp = this.getInitParameter("on-destroy-sexp");

            if(Logger.logger.isLoggable(Level.INFO)) {
                Logger.logger.info("Evaluating on-destroy-sexp for servlet : " + servletName);
            }

            this.eval(sexp);
        }
        catch(final MalformedURLException mue) {
            if(Logger.logger.isLoggable(Level.SEVERE)) {
                Logger.logger.severe(mue.getMessage());
            }
        }
        catch(final SchemeException se) {
            if(Logger.logger.isLoggable(Level.SEVERE)) {
                Logger.logger.severe(se.getMessage());
            }
        }
    }


    public void doGet(final HttpServletRequest request,
                      final HttpServletResponse response)
        throws ServletException
    {
        final String servletName = this.getServletName();

        try {
            RequestScope.setRequest(new SISCHttpServletRequest(request));
            RequestScope.setResponse(response);

            final ServletContext servletContext = this.getServletContext ();
            final AppContext appContext = AppContextLocator.lookup(servletContext);

            Context.execute(appContext, new SchemeCaller() {
                public Object execute(Interpreter r)
                    throws SchemeException
                {
                  Procedure sisclet = (Procedure) r.eval(Symbol.get("sisclet"));

                  return r.eval(sisclet, new Value[] {});
                } });
        }
        catch(final SchemeException e) {
            throw new ServletException(
				       this.contextName + ": sisclet call failed in servlet " + servletName + ": " + e.getMessage(),
				       sisc.modules.s2j.Util.javaException(e));
        }
        finally {
            RequestScope.setRequest(null);
            RequestScope.setResponse(null);
        }
    }


    public void doPost(final HttpServletRequest request,
                       final HttpServletResponse response)
        throws ServletException
    {
        doGet(request, response);
    }

    public void doPut(final HttpServletRequest request,
            final HttpServletResponse response)
    throws ServletException
    {
        doGet(request, response);
    }

    public void doDelete(final HttpServletRequest request,
            final HttpServletResponse response)
    throws ServletException
    {
        doGet(request, response);
    }

    protected void eval(String sexp)
        throws SchemeException, MalformedURLException
    {
        final ServletContext servletContext = this.getServletContext ();
        final AppContext appContext = AppContextLocator.lookup(servletContext);

        MiscUtil.eval(sexp, this.contextRoot, appContext, this.getServletName());
    }


    String extractContinuationId(HttpServletRequest request)
    {
        if(request.getRequestURI().contains("k-hash")) {
            Pattern p = Pattern.compile("(\\A.*)(;k-hash=)(\\p{Alnum}+)([;/].*\\z|.*\\z)");
            Matcher m = p.matcher(request.getRequestURI());
            return m.replaceAll("$3");
        }
        else {
            return null;
        }
    }
}
