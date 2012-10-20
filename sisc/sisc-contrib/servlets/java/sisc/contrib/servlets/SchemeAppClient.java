package sisc.contrib.servlets;

import java.net.*;
import java.io.*;
import sisc.data.*;
import sisc.io.*;

public class SchemeAppClient {

    public static void main(String[] args) throws Exception {

        URL url = new URL(args[0]);

        sisc.reader.Parser p = new sisc.reader.Parser(new sisc.reader.Lexer());
        InputPort inp = new ReaderInputPort(new BufferedReader(new InputStreamReader(System.in)));
        
        while(true) {
            System.out.print("\n> ");
            Value v = p.nextExpression(inp);

            HttpURLConnection conn = (HttpURLConnection)url.openConnection();
            conn.setRequestMethod("POST");
            conn.setRequestProperty("Content-Type", "application/x-scheme");
            conn.setDoInput(true);
            conn.setDoOutput(true);
            
            conn.connect();
            OutputStream out = conn.getOutputStream();
            OutputPort outp = new WriterOutputPort(new PrintWriter(out), true);
            ValueWriter w = new PortValueWriter(outp, false, false);
            w.write(v);
            outp.flush();
            InputStream in = conn.getInputStream();
            int i;
            while ((i = in.read()) != -1) {
                System.out.write(i);
            }
            conn.disconnect();
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
 * The Original Code is SISC Servlets.
 * 
 * The Initial Developer of the Original Code is Matthias Radestock.
 * Portions created by Matthias Radestock are Copyright (C) 2000-2002
 * Matthias Radestock.  All Rights Reserved.
 * 
 * Contributor(s):
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
