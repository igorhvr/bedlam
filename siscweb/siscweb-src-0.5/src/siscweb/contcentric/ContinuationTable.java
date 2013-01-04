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

import java.io.Serializable;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.logging.Level;

import javax.servlet.ServletContext;
import javax.servlet.http.HttpSession;
import javax.servlet.http.HttpSessionActivationListener;
import javax.servlet.http.HttpSessionBindingEvent;
import javax.servlet.http.HttpSessionBindingListener;
import javax.servlet.http.HttpSessionEvent;

import sisc.data.Procedure;
import siscweb.util.Environment;
import siscweb.util.Logger;
import siscweb.web.RequestScope;


/**
 * This class represents the table of continuations stored per each
 * user. Continuations are grouped in <code>ContinuationGroup</code>s,
 * each grouping continuations created during the same HTTP request.
 *
 * @see siscweb.web.SISCApplicationServlet
 *
 */
public class ContinuationTable
    implements Serializable,
               HttpSessionBindingListener, HttpSessionActivationListener
{
    private static final long serialVersionUID = 3688505502258902323L;

    private transient String sessionId;

    private ContinuationTable()
    { }


    /**
     * Returns a <code>ContinuationTable</code> instance associated to
     * the given session, creating the instance if necessary.
     *
     * @param session a user session
     */
    public static ContinuationTable getInstance(final HttpSession session)
    {
        ContinuationTable ct;

        synchronized(session) {
            ct = (ContinuationTable)
                    session.getAttribute("siscweb.continuation-table");

            if(ct == null) {
                ct = new ContinuationTable();
                session.setAttribute("siscweb.continuation-table", ct);
            }
        }

        return ct;
    }

    public String getSessionId()
    {
        return this.sessionId;
    }

    private void setSessionId(final String sessionId)
    {
        this.sessionId = sessionId;
    }

    /**
     * Stores a SISC <code>Procedure</code> in this continuation
     * table. A new continuation group is created if this is the first
     * continuation stored under the given <code>groupId</code>.
     *
     * If the number of already-stored continuation groups plus the
     * newly-created one exceeds the maximum allowed number
     * (&quot;siscweb/user-history-size&quot;), the continuation group
     * expiring the soonest is removed to make room for the new one.
     *
     * Also, the rate at which new continuation groups are created is
     * limited by the environment entry
     * &quot;siscweb/continuation-group-creation-interval&quot;.
     *
     * @param session a session
     * @param groupId the <code>ContinuationGroup</code> id to which
     *                this continuation should belong
     * @param continuationId the id under which to store this
     *                       continuation; this is the same identifier
     *                       used in URLs to recall a continuation
     * @param procedure the SISC <code>Procedure</code> to store
     * @param ttl the time-to-live left for this continuation in
     *            milliseconds
     */
    // TODO: concurrency can be increased by smarter locking on groups
    public synchronized void store(final String groupId,
                                   final String continuationId,
                                   final Procedure procedure,
                                   final long ttl,
                                   final Frame frame)
    {
        ContinuationGroup group = null;

        group = getGroup(groupId);

        if(group != null) {
            touchGroup(group);
        }
        else {
            waitGroupCreationInterval();
            group = new ContinuationGroup(groupId, new Long(ttl),
                                          getSessionId(), frame);
            addGroup(group);
        }

        trimGroups();

        addContinuation(continuationId, group, procedure);
    }


    /**
     * Retrieves a continuation from this continuation table. Once
     * retrieved, the group to which this continuation belongs is
     * &quot;touched&quot;, i.e. the time at which its continuations
     * expire is recalculated as the time-to-live starting from the
     * current time.
     *
     * @param session a user session
     * @param continuationId the identifier for the desired
     *                       continuation
     *
     * @returns a SISC <code>Procedure</code> previously stored under
     *          the given identifier, or <code>null</code> if one is
     *          not found.
     */
    public synchronized Procedure fetch(final String continuationId)
    {
        Continuation continuation = getContinuation(continuationId);

        if(continuation != null) {
            if(Logger.logger.isLoggable(Level.FINEST)) {
                Logger.logger.finest("Fetched continuation : " +
                        continuation.getId() + ", group : " +
                        continuation.getGroup().getId());
            }

            touchGroup(continuation.getGroup());

            return continuation.getProcedure();
        }
        else {
            return null;
        }
    }

    public Frame getFrame(String continuationId)
    {
        Continuation continuation = getContinuation(continuationId);

        if(continuation == null) {
            return null;
        }

        return continuation.getGroup().getFrame();
    }

    /**
     * Removes all continuations stored for the given session.
     *
     * @param session a user session
     */
    public synchronized void clear()
    {
        clearHelper();

        if(Logger.logger.isLoggable(Level.FINEST)) {
            Logger.logger.finest("Cleared all continuations for session : " +
                    getSessionId());
        }
    }


    /**
     * Removes all continuations past their time-to-live.
     *
     * @returns the number of continuation groups removed
     */
    public synchronized int purgeExpired()
    {
        Date now = new Date();
        int n = 0;

        for(Iterator i = groupTimeline.iterator(); i.hasNext();) {
            ContinuationGroup group = (ContinuationGroup) i.next();

            if(group.getExpirationDate().compareTo(now) < 0) {
                // pass the iterator so that the group can be safely
                // removed from the timeline; i know, hackish
                removeGroup(group, i);

                n++;
            }
            else {
                break; // no need to go further in the timeline
            }
        }

        if(Logger.logger.isLoggable(Level.FINER)) {
            Logger.logger.fine("Purged " + n +
                    " expired continuation groups from session " +
                    getSessionId());
        }

        return n;
    }


    // internal representation of continuation table;
    // none of the internal operations are synchronized because
    // the higher-level ones take care of that
    private final Map groups = new HashMap();
    private final SortedSet groupTimeline = new TreeSet();
    private final Map continuations = new HashMap();
    private long lastCreatedGroupTime = 0;


    private void addGroup(final ContinuationGroup group)
    {
        this.groups.put(group.getId(), group);
        this.groupTimeline.add(group);

        if(Logger.logger.isLoggable(Level.FINEST)) {
            Logger.logger.finest("Added continuation group : " +
                group.getId() + ", session : " + getSessionId());
        }
    }

    private ContinuationGroup getGroup(final String groupId)
    {
        return (ContinuationGroup) this.groups.get(groupId);
    }

    private void removeGroup(final ContinuationGroup group,
                             final Iterator timelineIterator)
    {
        groups.remove(group.getId());
        timelineIterator.remove();

        for(Iterator i = group.getContinuations().keySet().iterator(); i.hasNext();) {
            String continuationId = (String) i.next();

            this.continuations.remove(continuationId);
        }

        if(Logger.logger.isLoggable(Level.FINEST)) {
            Logger.logger.finest("Removed continuation group : " +
                    group.getId() + ", session : " + getSessionId());
        }
    }

    private void touchGroup(final ContinuationGroup group)
    {
        groupTimeline.remove(group);
        group.touch();
        groupTimeline.add(group);
    }

    private void clearHelper()
    {
        this.groups.clear();
        this.groupTimeline.clear();
        this.continuations.clear();
    }

    private void trimGroups()
    {
        final int maxHistorySize = Environment.getUserHistorySize();

        if(maxHistorySize == -1) {
            return;
        }

        this.trimGroups(maxHistorySize);
    }

    private int trimGroups(final int maxHistorySize)
    {
        int n = this.groupTimeline.size() - maxHistorySize;

        if(n > 0) {
            for(Iterator i = groupTimeline.iterator(); n > 0 && i.hasNext(); n--) {
                ContinuationGroup group = (ContinuationGroup) i.next();

                i.remove();
                groups.remove(group.getId());

                for(Iterator j = group.getContinuations().keySet().iterator(); j.hasNext();) {
                    String continuationId = (String) j.next();

                    this.continuations.remove(continuationId);
                }
            }

            if(Logger.logger.isLoggable(Level.FINEST)) {
                Logger.logger.finest("Trimmed : " + n +
                    " continuation groups from session : " +
                    getSessionId());
            }
        }

        return n;
    }

    private void waitGroupCreationInterval()
    {
        long now = new Date().getTime();
        long interval = now - this.lastCreatedGroupTime;
        long minInterval = Environment.getContinuationGroupCreationInterval();

        if(interval < minInterval) {
            try {
                long waitTime = minInterval  - interval;

                if(Logger.logger.isLoggable(Level.FINEST)) {
                    Logger.logger.finest("Waiting " +  waitTime / 1000 +
                            "ms before creating new continuation group for session : " +
                            getSessionId());
                }

                wait(waitTime);
            }
            catch(InterruptedException ie) {
                // do nothing
            }
        }

        this.lastCreatedGroupTime = new Date().getTime();
    }


    private void addContinuation(final String continuationId,
                                 final ContinuationGroup group,
                                 final Procedure procedure)
    {
        Continuation continuation =
                new Continuation(continuationId, group, procedure);

        group.addContinuation(continuation);

        if(Logger.logger.isLoggable(Level.FINEST)) {
            Logger.logger.finest("Added continuation : " + continuationId +
                    ", group : " + group.getId());
        }

        this.continuations.put(continuation.getId(), continuation);
    }

    private Continuation getContinuation(final String continuationId)
    {
        return (Continuation) this.continuations.get(continuationId);
    }


    // HttpSessionBindingListener inteface
    public void valueBound(final HttpSessionBindingEvent be)
    {
        final HttpSession session = be.getSession();
        final ServletContext servletContext = session.getServletContext();

        setSessionId(session.getId());
        this.register(servletContext);

        if(Logger.logger.isLoggable(Level.FINE)) {
            Logger.logger.fine("Continuation table bound to session : " +
                    getSessionId());
        }
    }

    public void valueUnbound(final HttpSessionBindingEvent be)
    {
        final HttpSession session = be.getSession();
        final ServletContext servletContext = session.getServletContext();

        if(Logger.logger.isLoggable(Level.FINE)) {
            Logger.logger.fine("Continuation table unbound from session : " +
                    getSessionId());
        }

        this.unregister(servletContext);
        setSessionId(null);
    }

    // HttpSessionActivationListener interface
    public void sessionDidActivate(final HttpSessionEvent se)
    {
        /* no contstore yet because siscinitlistener is run AFTER session deserialization */
        final HttpSession session = se.getSession();
        setSessionId(session.getId());

        if(Logger.logger.isLoggable(Level.FINE)) {
            Logger.logger.fine("Activated session : " +
                getSessionId());
        }

        this.register(session.getServletContext());
    }

    public void sessionWillPassivate(HttpSessionEvent se)
    {
        final HttpSession session = se.getSession();
        final ServletContext servletContext = session.getServletContext();

        this.unregister(servletContext);
        this.trimGroups(Environment.getUserHistorySizeSerialized());

        if(Logger.logger.isLoggable(Level.FINE)) {
            Logger.logger.fine("Passivated session : " +
                    getSessionId());
        }

        setSessionId(null);
    }

    protected void register(ServletContext servletContext)
    {
        ContinuationStore store =
                ContinuationStoreLocator.lookup(servletContext);

        if(store instanceof SessionContinuationStore) {
            ((SessionContinuationStore) store).register(this);

            if(Logger.logger.isLoggable(Level.FINE)) {
                Logger.logger.fine("Registered continuation table with continuation store for session : " +
                        getSessionId());
            }
        }
    }

    protected void unregister(ServletContext servletContext)
    {
        ContinuationStore store =
                ContinuationStoreLocator.lookup(servletContext);

        if(store instanceof SessionContinuationStore) {
            ((SessionContinuationStore) store).unregister(this);

            if(Logger.logger.isLoggable(Level.FINE)) {
                Logger.logger.fine("Unregistered continuation table from continuation store for session : " +
                        getSessionId());
            }
        }
    }
}
