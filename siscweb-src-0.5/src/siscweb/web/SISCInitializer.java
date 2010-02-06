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
 * Portions created by the Alessandro Colomba are Copyright (C) 2005-2006
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
import java.util.logging.Level;

import javax.servlet.ServletContext;
import javax.servlet.ServletContextEvent;
import javax.servlet.ServletContextListener;

import sisc.interpreter.AppContext;
import sisc.interpreter.SchemeException;
import siscweb.contcentric.AppContextLocator;
import siscweb.contcentric.ContinuationStore;
import siscweb.contcentric.ContinuationStoreLocator;
import siscweb.contcentric.ContinuationStoreThread;
import siscweb.util.Environment;
import siscweb.util.Logger;
import siscweb.util.MiscUtil;
import siscweb.util.REPLThread;


public class SISCInitializer implements ServletContextListener
{
    public void contextInitialized(final ServletContextEvent contextEvent)
    {
        final ServletContext servletContext = contextEvent.getServletContext();
        final String contextName = servletContext.getServletContextName();

        // initializes the logging system
        siscweb.util.Logger.initialize();

        // start the REPL; we don't fail if for some reason it doesn't run
        if(Environment.isREPL()) {
            try {
                startREPL(servletContext);
            }
            catch(final Exception e) {
                if(Logger.logger.isLoggable(Level.WARNING)) {
                    Logger.logger.warning(e.getMessage());
                }
            }
        }

        // eval on-init-sexp; we don't fail in case of errors,
        // as they can be fixed in the REPL
        try {
            if(Logger.logger.isLoggable(Level.INFO)) {
                Logger.logger.info("Evaluating siscweb.on-init-sexp for context : " + contextName);
            }

            final String sexp = servletContext.getInitParameter("siscweb.on-init-sexp");
            final AppContext appContext = AppContextLocator.lookup(servletContext);

            MiscUtil.eval(sexp, servletContext.getResource("/"), appContext, contextName);
        }
        catch(final MalformedURLException mue) {
            if(Logger.logger.isLoggable(Level.SEVERE)) {
                Logger.logger.severe(mue.getMessage());
            }
        }
        catch(SchemeException se) {
            if(Logger.logger.isLoggable(Level.WARNING)) {
                Logger.logger.warning(se.getMessage());
            }
        }

        // starts the continuation store thread
        startContinuationStoreThread(servletContext);
    }

    public void contextDestroyed(final ServletContextEvent contextEvent)
    {
        final ServletContext servletContext = contextEvent.getServletContext();
        final String contextName = servletContext.getServletContextName();

        // eval on-destroy-sexp
        try {
            if(Logger.logger.isLoggable(Level.INFO)) {
                Logger.logger.info("Evaluating siscweb.on-destroy-sexp for context : " + contextName);
            }

            final String sexp = servletContext.getInitParameter("siscweb.on-destroy-sexp");
            final AppContext appContext = AppContextLocator.lookup(servletContext);

            MiscUtil.eval(sexp, servletContext.getResource("/"), appContext, servletContext.getServletContextName());
        }
        catch(final MalformedURLException mue) {
            if(Logger.logger.isLoggable(Level.SEVERE)) {
                Logger.logger.severe(mue.getMessage());
            }
        }
        catch(SchemeException se) {
            if(Logger.logger.isLoggable(Level.WARNING)) {
                Logger.logger.warning(se.getMessage());
            }
        }

        this.stopContinuationStoreThread();
        this.stopREPL();
    }

    private REPLThread replThread = null;

    public void startREPL(final ServletContext servletContext)
    {
        final String host = Environment.getREPLHost();
        final int port = Environment.getREPLPort();

        if(host != null && port != 0) {
            final AppContext appContext = AppContextLocator.lookup(servletContext);

            this.replThread = new REPLThread(host, port, appContext);
            this.replThread.setDaemon(true);
            this.replThread.start();
        }
    }

    public void stopREPL()
    {
        if(this.replThread != null) {
            this.replThread.terminate();
        }
    }



    private ContinuationStoreThread continuationCleanupThread = null;

    public void startContinuationStoreThread(ServletContext context)
    {
        final ContinuationStore continuationStore = ContinuationStoreLocator.lookup(context);

        this.continuationCleanupThread = new ContinuationStoreThread(continuationStore);
        this.continuationCleanupThread.setDaemon(true);
        this.continuationCleanupThread.start();
    }

    public void stopContinuationStoreThread()
    {
        if(this.continuationCleanupThread != null) {
            this.continuationCleanupThread.terminate();
        }
    }
}
