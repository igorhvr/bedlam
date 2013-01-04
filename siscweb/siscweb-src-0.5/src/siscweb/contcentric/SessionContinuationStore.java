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

package siscweb.contcentric;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.logging.Level;

import javax.servlet.http.HttpSession;

import sisc.data.Procedure;
import siscweb.util.Environment;
import siscweb.util.Logger;

public class SessionContinuationStore implements ContinuationStore
{
    private final HashMap continuationTables = new HashMap();


    public void store(final HttpSession session,
                      final String groupId,
                      final String continuationId,
                      final Procedure procedure,
                      long ttl,
                      final Frame frame)
    {
        if(ttl < 0) {
            ttl = Environment.getContinuationTtl();
        }

        ContinuationTable.getInstance(session).store(groupId, continuationId, procedure, ttl, frame);
    }

    public Procedure fetch(final HttpSession session, final String continuationId)
    {
        ContinuationTable table = ContinuationTable.getInstance(session);

        if(table == null) {
            return null;
        }

        return table.fetch(continuationId);
    }

    public Frame getFrame(final HttpSession session, final String continuationId)
    {
        ContinuationTable table = ContinuationTable.getInstance(session);

        if(table == null) {
            return null;
        }

        return table.getFrame(continuationId);
    }

    public void clear(final HttpSession session)
    {
        ContinuationTable.getInstance(session).clear();
    }

    public int purgeExpired()
    {
        int n = 0;

        if(Logger.logger.isLoggable(Level.FINE)) {
            Logger.logger.fine("Purging expired continuations from all sessions.");
        }

        for(Iterator i = new ArrayList(this.continuationTables.keySet()).iterator(); i.hasNext();) {
            String sid = (String) i.next();
            ContinuationTable ct = (ContinuationTable) this.continuationTables.get(sid);

            // the table may have been unregistered in the meanwhile
            if(ct != null) {
                n += ct.purgeExpired();
            }
        }

        return n;
    }

    public HashMap getContinuationTables()
    {
        return this.continuationTables;
    }


    public void register(ContinuationTable ct)
    {
        this.continuationTables.put(ct.getSessionId(), ct);
    }

    public void unregister(ContinuationTable ct)
    {
        this.continuationTables.remove(ct.getSessionId());
    }
}
