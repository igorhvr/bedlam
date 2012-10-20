package sisc.io;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.UnsupportedEncodingException;

/**
 * Wrapper class which names a Character Set
 * 
 * @author scgmille
 *
 */
public class Charset {

    private String name;

    public static Charset forName(String name) throws UnsupportedEncodingException {
        new InputStreamReader(new ByteArrayInputStream(new byte[0]), name);
        Charset cs=new Charset();
        cs.name=name;
        return cs;
    }
    
    public String getName() {
        return name;
    }

    public String displayName() {
        return name;
    }

    public InputStreamReader newInputStreamReader(InputStream in) {
        try {
            return new InputStreamReader(in, getName());
        } catch (UnsupportedEncodingException use) {
            //Can't happen
            return null;
        }
    }

    public OutputStreamWriter newOutputStreamWriter(OutputStream out) {
        try {
            return new OutputStreamWriter(out, getName());
        } catch (UnsupportedEncodingException use) {
            //Can't happen
            return null;
        }
    }
}
/*
 * The contents of this file are subject to the Mozilla Public
 * License Version 1.1 (the "License"); you may not use this file
 * except in compliance with the License. You may obtain a copy of
 * the License at http://www.mozilla.org/MPL/
 * 
 * Software distributed under the License is distributed on an "AS
 * IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 * implied. See the License for the specific language governing
 * rights and limitations under the License.
 * 
 * The Original Code is the Second Interpreter of Scheme Code (SISC).
 * 
 * The Initial Developer of the Original Code is Scott G. Miller.
 * Portions created by Scott G. Miller are Copyright (C) 2000-2007
 * Scott G. Miller.  All Rights Reserved.
 * 
 * Contributor(s):
 * Matthias Radestock 
 * 
 * Alternatively, the contents of this file may be used under the
 * terms of the GNU General Public License Version 2 or later (the
 * "GPL"), in which case the provisions of the GPL are applicable 
 * instead of those above.  If you wish to allow use of your 
 * version of this file only under the terms of the GPL and not to
 * allow others to use your version of this file under the MPL,
 * indicate your decision by deleting the provisions above and
 * replace them with the notice and other provisions required by
 * the GPL.  If you do not delete the provisions above, a recipient
 * may use your version of this file under either the MPL or the
 * GPL.
 */
