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

package siscweb.util;

import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Level;

import javax.naming.InitialContext;
import javax.naming.NamingException;


public class Environment
{
    public static boolean isREPL()
    {
        boolean isREPL = lookupBoolean("siscweb/repl-p", false);

        return isREPL;
    }

    public static String getREPLHost()
    {
        return lookupString("siscweb/repl-host", null);
    }

    public static int getREPLPort()
    {
        return lookupInt("siscweb/repl-port", 0);
    }

    public static String getLoggingLevel()
    {
        return lookupString("siscweb/logging-level", "INFO");
    }

    public static String getLoggingFile()
    {
        return lookupString("siscweb/logging-file", "");
    }

    public static int getLoggingLimit()
    {
        return lookupInt ("siscweb/logging-limit", 5000000);
    }

    public static int getLoggingCount()
    {
        return lookupInt ("siscweb/logging-count", 1);
    }


    public static String getContinuationStoreClassName()
    {
        return lookupString("siscweb/continuation-store-class", "siscweb.contcentric.SessionContinuationStore");
    }


    public static long getContinuationTtl()
    {
        return lookupLong("siscweb/continuation-ttl", 1200);
    }

    public static int getUserHistorySize()
    {
        return lookupInt("siscweb/user-history-size", -1);
    }

    public static int getUserHistorySizeSerialized()
    {
        return lookupInt("siscweb/user-history-size-serialized", -1);
    }

    public static long getContinuationGroupCreationInterval()
    {
        return lookupInt("siscweb/continuation-group-creation-interval", 0);
    }

    public static long getContinuationCleanupInterval()
    {
        return lookupLong("siscweb/continuation-cleanup-interval", 30000L);
    }


    public static boolean lookupBoolean(final String name, final boolean defaultValue)
    {
        boolean useDefaultValue = false;
        Boolean value = null;

        try {
            value = (Boolean) lookupMemoized(name);

            if(value == null) {
                useDefaultValue = true;
            }
        }
        catch(final NamingException ne) {
            useDefaultValue = true;
        }
        catch(final Exception e) {
            if(Logger.logger.isLoggable(Level.WARNING)) {
                Logger.logger.finest("Error retrieving " + name + " enviroment entry.");
            }

            useDefaultValue = true;
        }

        if(useDefaultValue == false) {
            return value.booleanValue();
        }
        else {
            if(Logger.logger.isLoggable(Level.FINEST)) {
                Logger.logger.finest("Using default value " + name + " : " + defaultValue);
            }

            return defaultValue;
        }
    }

    public static int lookupInt(final String name, final int defaultValue)
    {
        boolean useDefaultValue = false;
        Integer value = null;

        try {
            value = (Integer) lookupMemoized(name);

            if(value == null) {
                useDefaultValue = true;
            }
        }
        catch(final NamingException ne) {
            useDefaultValue = true;
        }
        catch(final Exception e) {
            if(Logger.logger.isLoggable(Level.WARNING)) {
                Logger.logger.finest("Error retrieving " + name + " enviroment entry.");
            }

            useDefaultValue = true;
        }

        if(useDefaultValue == false) {
            return value.intValue();
        }
        else {
            if(Logger.logger.isLoggable(Level.FINEST)) {
                Logger.logger.finest("Using default value " + name + " : " + defaultValue);
            }

            return defaultValue;
        }
    }

    public static long lookupLong(final String name, final long defaultValue)
    {
        boolean useDefaultValue = false;
        Long value = null;

        try {
            value = (Long) lookupMemoized(name);

            if(value == null) {
                useDefaultValue = true;
            }
        }
        catch(final NamingException ne) {
            useDefaultValue = true;
        }
        catch(final Exception e) {
            if(Logger.logger.isLoggable(Level.WARNING)) {
                Logger.logger.finest("Error retrieving " + name + " enviroment entry.");
            }

            useDefaultValue = true;
        }

        if(useDefaultValue == false) {
            return value.longValue();
        }
        else {
            if(Logger.logger.isLoggable(Level.FINEST)) {
                Logger.logger.finest("Using default value " + name + " : " + defaultValue);
            }

            return defaultValue;
        }
    }

    public static String lookupString(final String name, final String defaultValue)
    {
        boolean useDefaultValue = false;
        String value = null;

        try {
            value = (String) lookupMemoized(name);

            if(value == null) {
                useDefaultValue = true;
            }
        }
        catch(final NamingException ne) {
            useDefaultValue = true;
        }
        catch(final Exception e) {
            if(Logger.logger.isLoggable(Level.WARNING)) {
                Logger.logger.finest("Error retrieving " + name + " enviroment entry.");
            }

            useDefaultValue = true;
        }

        if(useDefaultValue == false) {
            return value.trim();
        }
        else {
            if(Logger.logger.isLoggable(Level.FINEST)) {
                Logger.logger.finest("Using default value " + name + " : " + defaultValue);
            }

            return defaultValue;
        }
   }


    private static Map memo = new HashMap();     // memoized values
    private static Map memoTime = new HashMap(); // time of last lookup, even if it failed
    public static long memoInterval = 60000;     // interval between lookups; left public for hacking

    public static Object lookupMemoized(final String name)
        throws NamingException
    {
        // NOTE: we don't care much about concurrency; worst case,
        //       the timestamps will be slightly off
        final Date ts = (Date) memoTime.get(name);

        if(ts == null || new Date().getTime() - ts.getTime() > memoInterval) {
            memoTime.put(name, new Date());

            final Object o = lookup(name);

            memo.put(name, o);

            return o;
        }
        else {
            return memo.get(name);
        }
    }

    public static Object lookup(final String name)
        throws NamingException
    {
        try {
            final InitialContext context = new InitialContext();
            return context.lookup("java:comp/env/" + name);
        }
        catch(final NamingException ne) {
            // entries are optional; if one is missing, record the
            // fact without fuss
            if(Logger.logger.isLoggable(Level.FINEST)) {
                Logger.logger.finest("Env entry \"" + name + "\" not defined.");
            }

            throw ne;
        }
    }
}
