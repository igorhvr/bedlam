import java.awt.*;
import javax.swing.*;
import sisc.data.*;
import sisc.util.Util;

public class Gui extends JFrame {

    Interpreter r;
    int ww, wh;
    String world;
    int fw=7,fh=10;
    int id;

    public Arena a;

    public Gui(int id) {
        super("Arena");
        this.id=id;
        r=Context.enter();
        try {
            ww=((Quantity)r.eval("world-width")).intValue();
            wh=((Quantity)r.eval("world-height")).intValue();
            world=((SchemeString)r.eval("world-map")).asString();
            getContentPane().add(a=new Arena(ww, wh));
            pack();
        } catch (Exception se) {
            se.printStackTrace();
        }
    }

    class Arena extends Panel {
        protected Image offscreen;
        protected int w,h;
        
        public Arena(int w, int h) {
            this.w=w*fw;
            this.h=h*fh;
        }
        
        public Graphics getGraphicsContext() {
            return (offscreen=
                    createImage(getSize().width, getSize().height)).getGraphics();
        }
        
        public Dimension getPreferredSize() {
            return new Dimension(w, h);
            
        }
        
        public void paint(Graphics g) {
            Pair path=Util.EMPTYLIST;
            int rx=1,ry=1;
            try {
                Pair robot=(Pair)r.eval("(robot-position "+id+")");
                path=(Pair)r.eval("last-path");
                rx=((Quantity)robot.car()).intValue();
                ry=((Quantity)((Pair)robot.cdr()).car()).intValue();
            } catch (Exception e) {
                e.printStackTrace();
            }
            g.setFont(new Font("Monospaced",Font.BOLD,12));
            //            System.err.println(ww+","+wh);
            for (int j=wh; j>0; j--) {
                int yp = (wh-j)*fh;
                int sp=(j-1)*ww;
                String toDraw=world.substring(sp,sp+ww);
                if (ry==j)
                    toDraw=toDraw.substring(0,rx-1)+
                        "*"+toDraw.substring(rx);
                g.drawString(toDraw, 0, yp+fh);
            }
            while (path!=Util.EMPTYLIST) {
                Pair pe=(Pair)path.car();
                int x = ((Quantity)pe.car()).intValue();
                int y = ((Quantity)((Pair)pe.cdr()).car()).intValue();
                g.setColor(Color.green);
                g.drawString("0", fw*(x-1), fh*(wh-(y-1)));
                path=(Pair)path.cdr();
            }
        } 
    }
}
            
    
