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

import java.util.logging.Level;

import javax.servlet.http.HttpSession;

import sisc.data.Procedure;
import siscweb.util.Logger;


public interface ContinuationStore
{
    public abstract void store(final HttpSession session,
                               final String groupId,
                               final String closureId,
                               final Procedure procedure,
                               final long ttl,
                               final Frame frame);

    public abstract Procedure fetch(final HttpSession session,
                                    final String continuationId);

    public abstract void clear(final HttpSession session);

    public abstract int purgeExpired();

    public Frame getFrame(final HttpSession session, final String continuationId);

    public class Factory
    {
        public static ContinuationStore create(final String className)
        {
            try {
                final Class c = Class.forName(className);

                if(Logger.logger.isLoggable(Level.INFO)) {
                    Logger.logger.info("Instantiating class \"" + className + "\" for storing continuations.");
                }

                ContinuationStore continuationStore = (ContinuationStore) c.newInstance();


                return continuationStore;
            }
            catch(final ClassNotFoundException cnfe)
            {
                if(Logger.logger.isLoggable(Level.SEVERE)) {
                    Logger.logger.severe("Continuation store class \"" + className + "\" could not be located.");
                }

                throw new RuntimeException(cnfe);
            }
            catch(final IllegalAccessException iae)
            {
                if(Logger.logger.isLoggable(Level.SEVERE)) {
                    Logger.logger.severe("Continuation store class \"" + className + "\" or its nullary constructor is not accessible.");
                }

                throw new RuntimeException(iae);
            }
            catch(final InstantiationException ie)
            {
                if(Logger.logger.isLoggable(Level.SEVERE)) {
                    Logger.logger.severe("Continuation store class \"" + className + "\" could not be instantiated.");
                }

                throw new RuntimeException(ie);
            }
        }
    }
}
