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

package siscweb.util;

import java.io.IOException;
import java.net.InetAddress;
import java.net.ServerSocket;
import java.net.UnknownHostException;
import java.util.logging.Level;

import sisc.REPL;
import sisc.interpreter.AppContext;

public class REPLThread extends Thread
{
    private String host;
    private int port;
    private AppContext appCtx;

    protected ServerSocket socket;

    public REPLThread(final String host, final int port, final AppContext appCtx)
    {
        this.host = host;
        this.port = port;
        this.appCtx = appCtx;
    }

    public void run()
    {
        try {
            final InetAddress addr = InetAddress.getByName(this.host);

            this.socket =  new ServerSocket(port, 50, addr);

            if(Logger.logger.isLoggable(Level.INFO)) {
                Logger.logger.info("Starting REPL on " + host + ":" + port);
            }

            REPL.listen(appCtx, this.socket);
        }
        catch(final UnknownHostException e) {
            throw new RuntimeException(
                "REPL could not resolve " +
                this.host, e);
        }
        catch(final IOException e){ /* alles gut? */ }
    }

    public void terminate()
    {
        if(Logger.logger.isLoggable(Level.INFO)) {
            Logger.logger.info("Stopping REPL on " + host + ":" + port);
        }

        try {
            this.interrupt();
        }
        finally {
            try {
                if(this.socket != null) {
                    this.socket.close();
                    this.socket = null;
                }
            }
            catch(final IOException e) {
                if(Logger.logger.isLoggable(Level.WARNING)) {
                    Logger.logger.warning("REPL could not close socket at " +
                            this.host + ":" + this.port);
                }
            }
        }
    }
}
