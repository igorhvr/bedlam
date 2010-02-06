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

import java.io.Serializable;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Level;

import siscweb.util.Logger;
import siscweb.web.RequestScope;


public class ContinuationGroup implements Comparable, Serializable
{
    private static final long serialVersionUID = -3448029499944547457L;
    private String id;
    private Long ttl;
    private String sessionId;
    private Date creationDate;
    private Date visitDate;
    private Date expirationDate;
    private Map continuations;

    private Frame frame;

    public ContinuationGroup ()
    { }

    public ContinuationGroup(final String id, final Long ttl, final String sessionId, final Frame frame)
    {
        this.id = id;
        this.creationDate = new Date();
        this.ttl = ttl;
        this.sessionId = sessionId;
        this.frame = frame;

        this.expirationDate = new Date(new Date().getTime() + 1000 * ttl.intValue());
        this.continuations = new HashMap();

        if(Logger.logger.isLoggable(Level.FINEST)) {
            Logger.logger.finest("Created continuation group id : " +
                    this.getId() + ", session : " + this.getSessionId() +
                    ", ttl : " + this.getTtl());
        }
    }

    public void touch()
    {
        final Date now = new Date();
        setVisitDate(now);

        if(Logger.logger.isLoggable(Level.FINEST)) {
            final long remainingTtl = (this.expirationDate.getTime() - now.getTime()) / 1000;

            Logger.logger.finest("Touched continuation group id : " +
                    this.getId()+ ", session : " + this.getSessionId() +
                    ", ttl : " + ttl + ", remaining-ttl : " + remainingTtl);
        }

        setExpirationDate(new Date(now.getTime() + 1000 * getTtl().longValue()));
    }

    public void addContinuation(Continuation continuation)
    {
        continuation.setGroup(this);
        this.continuations.put(continuation.getId(), continuation);

        if(Logger.logger.isLoggable(Level.FINEST)) {
            Logger.logger.finest("Added continuation id : " + continuation.getId() +
                    ", group : " + this.getId());
        }
    }

    public String getId()
    {
        return id;
    }

    private void setId(final String id)
    {
        this.id = id;
    }

    public Date getCreationDate()
    {
        return creationDate;
    }

    private void setCreationDate(final Date creationDate)
    {
        this.creationDate = creationDate;
    }

    public Date getExpirationDate()
    {
        return expirationDate;
    }

    private void setExpirationDate(final Date expirationDate)
    {
        this.expirationDate = expirationDate;
    }

    public Long getTtl()
    {
        return ttl;
    }

    public void setTtl(final Long ttl)
    {
        this.ttl = ttl;
    }

    public Date getVisitDate()
    {
        return visitDate;
    }

    private void setVisitDate(final Date visitDate)
    {
        this.visitDate = visitDate;
    }

    public Frame getFrame()
    {
        return this.frame;
    }

    public void setFrame(Frame frame)
    {
        this.frame = frame;
    }


    public String getSessionId()
    {
        return sessionId;
    }

    private void setSessionId(final String sessionId)
    {
        this.sessionId = sessionId;
    }

    public Map getContinuations()
    {
        return this.continuations;
    }

    private void setContinuations(final Map continuations)
    {
        this.continuations = continuations;
    }

    public int compareTo(final Object obj)
    {
        ContinuationGroup group = (ContinuationGroup) obj;

        return this.getExpirationDate().compareTo(group.getExpirationDate());
    }
}
