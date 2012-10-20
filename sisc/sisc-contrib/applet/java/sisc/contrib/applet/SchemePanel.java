package sisc.contrib.applet;

import javax.swing.*;
import javax.swing.event.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.text.*;
import java.awt.font.*;
import java.io.*;

import sisc.*;
import sisc.data.*;
import sisc.io.*;

import sisc.interpreter.AppContext;
import sisc.env.DynamicEnvironment;

public class SchemePanel extends JScrollPane {
    protected static final int
        SYS=0, USER=1, RESULT=2, ERROR=3;

    protected JTextPane disp;
    protected SchemeDocument sd;
    protected PipedOutputStream interpin;
    protected InputStream iis;
    protected DocumentOutputStream dos;
    protected int currentEditablePos;
    protected REPL repl;

    public DynamicEnvironment dynenv;
    
    public SchemePanel(AppContext ctx, SchemeDocument sd, JTextPane disp) {
        super(disp, VERTICAL_SCROLLBAR_ALWAYS, HORIZONTAL_SCROLLBAR_AS_NEEDED);
        dos=new DocumentOutputStream(sd, sd.siscText, disp);
        this.sd=sd;
        this.disp=disp;
        interpin=new PipedOutputStream();
        try {
            iis=new PipedInputStream(interpin);
        } catch (IOException e) {}
        
        dynenv = new DynamicEnvironment(ctx, iis, dos);
        disp.setEditable(false);
        currentEditablePos=sd.getLength()-1;


        repl=new REPL(dynenv, REPL.getCliProc(ctx));
        repl.go();
    }

    public Dimension getPreferredSize() {
        return new Dimension(600,400);
    }

    public void addMessage(int style, String m) {
        sd.addMessage(style, m, true);
    }

    public void addMessageNoNewline(int style, String m) {
        sd.addMessage(style, m, false);
    }

    public void eval(String s) {
        addMessage(USER, s);
        try {
            interpin.write((s+" ").getBytes());
            interpin.flush();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
    
    static class DocumentOutputStream extends OutputStream {
        protected Document doc;
        protected Style style;
        protected JTextComponent comp;

        public DocumentOutputStream(Document d, Style s, JTextComponent c) {
            doc=d;
            style=s;
            comp=c;
        }

        public void write(int b) throws IOException {
            try {
                doc.insertString(doc.getLength(), ""+(char)b, style);
                comp.setCaretPosition(doc.getLength()-1);
            } catch (BadLocationException ble) {}
        }

        public void write(byte[] b, int offset, int len) {
            try {
                String s=new String(b, offset, len);
                doc.insertString(doc.getLength(), s, style);
                comp.setCaretPosition(doc.getLength()-1);
            } catch (BadLocationException ble) {}
        }
    }
		
    static class SchemeDocument extends DefaultStyledDocument {
        Style masterSettings=addStyle(null,null);
        Style userText, siscText, siscResultText, siscErrorText;
        String myId;

        public SchemeDocument() {
            StyleConstants.setFontSize(masterSettings, 16);
            userText=addStyle(null,masterSettings);
            siscText=addStyle(null,masterSettings);
            siscResultText=addStyle(null,masterSettings);
            siscErrorText=addStyle(null,masterSettings);

            StyleConstants.setFontFamily(siscText, "Courier");
            StyleConstants.setFontFamily(siscResultText, "Helvetica");
            StyleConstants.setForeground(userText, Color.gray);
            StyleConstants.setForeground(siscText, Color.black);
            StyleConstants.setForeground(siscResultText, Color.blue);
            StyleConstants.setForeground(siscErrorText, Color.red);
        }

        public synchronized void addMessage(int style, 
                                            String message, boolean newline) {
            try {
                Style s=null;
                switch(style) {
                case SYS: s=siscText; break;
                case USER: s=userText; break;
                case RESULT: s=siscResultText; break;
                case ERROR: s=siscErrorText; break;
                }

                if (newline) message+="\n";
                insertString(getLength(), message, s);
            } catch (Exception e){}
        }
    }
}
    
