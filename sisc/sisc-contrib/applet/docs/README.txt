SISC Applet
-----------

This contributed module builds a .jar that can be placed on a webpage along
with a corresponding sisc.jar and sisc.shp built from the official SISC
module (or from a pre-built release).  To add SISC to your webpage, you want
to place the following tag on your site:

<applet code="sisc.contrib.applet.SISCApplet"
        archive="sisc-applet.jar,sisc.jar" 
        width="700" height="530">
</applet> 

Your users must have the Java Plugin v1.2 or higher running for this to
work.
