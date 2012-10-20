package sisc.contrib.applet;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.net.*;
import java.util.zip.GZIPInputStream;

import sisc.ser.*;
import sisc.interpreter.*;
import sisc.REPL;

public class SISCApplet extends JApplet {

    public void init() {
        try {
            SISCFrame sf=new SISCFrame(Context.getDefaultAppContext());
            //	sf.pack();
            //	sf.show();
            getContentPane().add(sf);
            sf.input.grabFocus();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}

