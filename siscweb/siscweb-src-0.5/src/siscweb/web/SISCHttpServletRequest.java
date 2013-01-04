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

package siscweb.web;

import java.util.Enumeration;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletRequestWrapper;

import siscweb.util.BindingList;


public class SISCHttpServletRequest extends HttpServletRequestWrapper
{
    public SISCHttpServletRequest(final HttpServletRequest request)
    {
        super(request);
        this.loadAttributeNames();
    }

    public SISCHttpServletRequest(final HttpServletRequest request,
                                  final Map bindings)
    {
        super(request);
        this.loadAttributeNames();

        for(Iterator i = bindings.entrySet().iterator(); i.hasNext();) {
            final Map.Entry entry = (Map.Entry) i.next();
            final String name = (String) entry.getKey();
            final Object value = entry.getValue();

            this.setAttribute(name, value);
        }
    }

    public Object getBinding(final String name)
    {
        final Object value = this.lookupBinding(name);

        if(value == null) {
            return null;
        }

        if(value instanceof BindingList) {
            final BindingList values = (BindingList) value;

            return values.size() > 0 ? values.get(0) : null;
        }
        else if(value instanceof Object[]) {
            final Object[] values = (Object[]) value;

            return values.length > 0 ? values[0] : null;
        }
        else {
            return value;
        }
    }

    public Object[] getBindingValues(final String name)
    {
        final Object value = this.lookupBinding(name);

        if(value == null) {
            return new Object[0];
        }

        if(value instanceof BindingList) {
            final BindingList values = (BindingList) value;

            return values.toArray(new Object[0]);
        }
        else if(value instanceof Object[]) {
            final Object[] values = (Object[]) value;

            return values;
        }
        else {
            return new Object[] { value };
        }
    }

    public Set getBindingNames()
    {
        Set names = new HashSet();

        names.addAll(this.getParameterMap().keySet());

        for(Enumeration e = this.getAttributeNames(); e.hasMoreElements();) {
            String name = (String) e.nextElement();

            names.add(name);
        }

        return names;
    }

    public Map getBindingMap()
    {
        Map bindings = new HashMap();
        bindings.putAll(this.getParameterMap());

        for(Enumeration e = this.getAttributeNames(); e.hasMoreElements();) {
            String name = (String) e.nextElement();

            bindings.put(name, this.getAttribute(name));
        }

        return bindings;
    }

    public void setAttribute(String name, Object value)
    {
        if(value == null) {
            this.attributeNames.add(name);
        }

        super.setAttribute(name, value);
    }

    public void removeAttribute(String name)
    {
        this.attributeNames.add(name);

        super.removeAttribute(name);
    }

    protected Set attributeNames = new HashSet();

    protected void loadAttributeNames()
    {
        for(Enumeration e = this.getAttributeNames(); e.hasMoreElements();) {
            this.attributeNames.add(e.nextElement());
        }
    }

    protected Object lookupBinding(String name)
    {
        Object value = this.getAttribute(name);

        if(value == null && !this.attributeNames.contains(name))
        {
            value = this.getParameter(name);
        }

        return value;
    }
}
