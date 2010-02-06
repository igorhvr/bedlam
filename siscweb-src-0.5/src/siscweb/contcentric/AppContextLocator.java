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

package siscweb.contcentric;

import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;
import java.util.logging.Level;

import javax.servlet.ServletContext;

import sisc.data.Symbol;
import sisc.interpreter.AppContext;
import sisc.interpreter.Context;
import sisc.interpreter.Interpreter;
import sisc.interpreter.SchemeCaller;
import sisc.interpreter.SchemeException;
import siscweb.util.Logger;


public class AppContextLocator
{
    public static AppContext lookup(ServletContext servletContext)
    {
        AppContext appContext = (AppContext)
            servletContext.getAttribute("siscweb.app-context");

        if(appContext == null) {
            synchronized(servletContext) {
                // checks again in case another thread set it
                appContext = (AppContext) servletContext.getAttribute("siscweb.app-context");

                if(appContext == null) {
                    if(Logger.logger.isLoggable(Level.FINE)) {
                        Logger.logger.fine("Creating SISC AppContext in Servlet context : " +
                                servletContext.getServletContextName());
                    }

                    appContext = Factory.create(servletContext);

                    servletContext.setAttribute("siscweb.app-context", appContext);
                }
            }
        }

        return appContext;
    }

    static class Factory
    {
        static AppContext create(final ServletContext servletContext)
        {
            final String contextName = servletContext.getServletContextName();
            final Properties siscProperties = getSISCProperties(servletContext, contextName);
            // instantiates and registers sisc context;
            final AppContext appContext = new AppContext(siscProperties);

            try {
                if(Logger.logger.isLoggable(Level.INFO)) {
                    Logger.logger.info("Initializing SISC interpreter in context : " + contextName);
                }

                appContext.addDefaultHeap();

                Context.execute(appContext, new SchemeCaller() {
                    public Object execute(Interpreter r)
                        throws SchemeException
                    {
                        r.define(Symbol.get ("*SISCWEB.SERVLET-CONTEXT*"),
                                new sisc.modules.s2j.JavaObject(servletContext),
                                sisc.util.Util.TOPLEVEL);

                        return null;
                        }
                   });

                return appContext;
            }
            catch(final IOException ioe) {
                if(Logger.logger.isLoggable(Level.SEVERE)) {
                    Logger.logger.severe("Error loading SISC heap in context : " + contextName);
                }

                throw new RuntimeException(ioe);
            }
            catch(final SchemeException se) {
                if(Logger.logger.isLoggable(Level.SEVERE)) {
                    Logger.logger.severe("Error providing ServletContext to SISC runtime in context : " + contextName);
                }

                throw new RuntimeException(se);
            }
        }

        // tries to load sisc properties from WEB-INF/sisc.properties
        private static Properties getSISCProperties(final ServletContext servletContext,
                                                    final String contextName)
        {
            final Properties siscProperties = new Properties();
            InputStream is = null;

            try {
                is = servletContext.getResourceAsStream("/WEB-INF/sisc.properties");

                if(is != null) {
                    siscProperties.load(is);
                    if(Logger.logger.isLoggable(Level.INFO)) {
                        Logger.logger.info("Loading WEB-INF/sisc.properties : " + contextName);
                    }
                }
            }
            catch(IOException ioe) {
                if(Logger.logger.isLoggable(Level.WARNING)) {
                    Logger.logger.warning("Error reading WEB-INF/sisc.properties : " + contextName);
                }
            }
            finally {
                if(is != null) {
                    try { is.close(); } catch (IOException ioe) {}
                }
            }
            return siscProperties;
        }
    }
}
