package sisc.contrib.applet;

import javax.swing.*;
import java.awt.event.*;
import java.awt.*;
import java.io.*;

import sisc.data.*;
import sisc.io.*;

import javax.swing.Box;
import sisc.util.Util;
import sisc.interpreter.AppContext;

public class SISCFrame extends JPanel implements ActionListener, KeyListener {

    protected SchemePanel sp;
    public JTextArea input;
    protected JButton eval, clear;
    protected JCheckBox autoClear, submitOnEnter;

    public SISCFrame(AppContext ctx) {
        setLayout(new BorderLayout());
        SchemePanel.SchemeDocument d=new SchemePanel.SchemeDocument();
        sp=new SchemePanel(ctx, d, new JTextPane(d));
        input=new JTextArea(4,70);
        input.setText("; Enter s-expressions here");
        JSplitPane split=new JSplitPane(JSplitPane.VERTICAL_SPLIT, sp, input);
        JPanel execPanel=new JPanel();
        execPanel.setLayout(new BoxLayout(execPanel, BoxLayout.X_AXIS));
        execPanel.add(Box.createHorizontalGlue());
        eval=new JButton("Evaluate");
        clear=new JButton("Clear");
        autoClear=new JCheckBox("Auto-Clear");
        submitOnEnter=new JCheckBox("Evaluate on Enter");
        autoClear.setSelected(true);
        submitOnEnter.setSelected(true);
        execPanel.add(submitOnEnter);
        execPanel.add(autoClear);
        execPanel.add(clear);
        execPanel.add(eval);
        add(split, BorderLayout.CENTER);
        add(execPanel, BorderLayout.SOUTH);
        eval.addActionListener(this);
        clear.addActionListener(this);
        input.addKeyListener(this);
        /*	addWindowListener(new WindowAdapter() {
            public void windowClosing(WindowEvent e) {
		    System.exit(0);
            }
            });*/
    }
    /*
      public void show() {
      super.show();
      input.grabFocus();
      }
    */
    public void keyPressed(KeyEvent e) {}
    public void keyReleased(KeyEvent e) {}
    public void keyTyped(KeyEvent e) {
        if (submitOnEnter.isSelected() && e.getKeyChar()=='\n' &&
            e.getKeyCode()==0)
            actionPerformed(new ActionEvent(input, 0, "Evaluate"));
    }

    public void actionPerformed(ActionEvent e) {
        if (e.getActionCommand().equals("Clear"))
            input.setText("");
        else {
            String s=input.getText().trim();
            if (s.length()>1 && s.charAt(s.length()-1)=='\n')
                s=s.substring(0,s.length()-1);
            PushbackReader ip=new PushbackReader(new BufferedReader(new StringReader(s)));
            Value v=Util.VOID;
            try {
                v=sp.dynenv.parser.nextExpression(ip); 
            } catch (EOFException eof) {
                //eof.printStackTrace();
                return;
            } catch (Exception ie) {
                System.err.println(ie);
            }
            if (autoClear.isSelected())
                input.setText("");
            sp.eval(s);
        }
    }
}
