package sisc.contrib.servlets;

import javax.servlet.http.*;
import javax.servlet.*;
import java.io.*;
import java.net.*;

import sisc.*;
import sisc.data.*;
import sisc.ser.*;
import sisc.interpreter.*;
import sisc.io.*;

public class SchemeAppServlet extends SchemeServletBase {

    SocketListener socketListener = null;

    public void init() throws ServletException {
        super.init();
        String initExpr = getInitParameter("init-expr");
        evalExpr(initExpr);

        ServletContext sctx = getServletContext();
        String port = sctx.getInitParameter("repl-port");
        if (port == null) return;
        InetAddress bindAddr = null;
        String host = sctx.getInitParameter("repl-host");
        if (host != null) {
            try {
                bindAddr = InetAddress.getByName(host);
            } catch (UnknownHostException e) {
                throw new ServletException("unable to bind to host " + host, e);
            }
        }
        try {
            ServerSocket ssocket = new ServerSocket(Integer.parseInt(port),
                                                    50,
                                                    bindAddr);
            socketListener = new SocketListener(ssocket);
            socketListener.start();
            log("repl listening on " + (host == null ? "*" : host) + ":" + port);
        } catch (IOException e) {
            throw new ServletException("unable to listen", e);
        }
    }

    public void destroy() {
        String destroyExpr = getInitParameter("destroy-expr");
        try {
            evalExpr(destroyExpr);
        } catch (ServletException e) {
            throw new RuntimeException(e.toString());
        }
        if (socketListener != null) {
            socketListener.interrupt();
            try {
                socketListener.ssocket.close();
            } catch (IOException e) {}
            socketListener = null;
        }
    }

    private String createResponse(String requestText, String responseText) {
        return "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\"><html><head><title>Scheme Application Administration</title></head><body><h1>Scheme Application Administration</h1><form method=\"POST\"><p><textarea name=\"request\" rows=\"10\" cols=\"80\">" + searchReplace(requestText, htmlEscapes) + "</textarea></p><p><input type=\"SUBMIT\" value=\"Evaluate\"></input></p><p><textarea name=\"response\" rows=\"10\" cols=\"80\">" + searchReplace(responseText, htmlEscapes) +"</textarea></p></form></body></html>";
    }
        
    public void doGet(HttpServletRequest request,
                      HttpServletResponse response)
        throws IOException, ServletException {
        response.setContentType("text/html");
        PrintWriter writer = response.getWriter();
        writer.print(createResponse("",""));
        return;
    }

    public void doPost(HttpServletRequest request,
                       HttpServletResponse response)
        throws IOException, ServletException {

        String ct = request.getContentType();


        if ("application/x-scheme".equals(ct))
            streamPost(request, response);
        else
            formPost(request, response);
    }
        
    public void formPost(HttpServletRequest request,
                         HttpServletResponse response)
        throws IOException, ServletException {

        String req = request.getParameter("request");
        Interpreter r = Context.enter();
        String resp;
        try {
            resp = r.eval(req).toString();
        } catch (SchemeException e) {
            resp = e.getMessage();
        } finally {
            Context.exit();
        }
        
        response.setContentType("text/html");
        PrintWriter writer = response.getWriter();
        writer.print(createResponse(req, resp));
        return;
    }

    public void streamPost(HttpServletRequest request,
                           HttpServletResponse response)
        throws IOException, ServletException {

        sisc.reader.Parser p = new sisc.reader.Parser(new sisc.reader.Lexer());
        InputPort inp = new ReaderInputPort(new BufferedReader(new InputStreamReader(request.getInputStream())));

        Interpreter r = Context.enter();
        try {
            Value v = r.eval(p.nextExpression(inp));
            response.setContentType("application/x-scheme");
            OutputPort outp = new WriterOutputPort(response.getWriter(), true);
            ValueWriter w = new PortValueWriter(outp,
                                                r.dynenv.vectorLengthPrefixing,
                                                r.dynenv.caseSensitive);
            w.write(v);
            outp.flush();
        } catch (SchemeException e) {
            response.getWriter().print(e.getMessage());
        } finally {
            Context.exit();
        }
        
    }
}

class SocketListener extends Thread {

    public ServerSocket ssocket;

    SocketListener(ServerSocket ssocket) {
        this.ssocket = ssocket;
    }

    public void run() {
        try {
            REPL.listen(Context.currentAppContext(), ssocket);
            ssocket.close();
        } catch (IOException e) {}
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
