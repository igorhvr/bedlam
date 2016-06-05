(require-extension (lib iasylum/jcode))

(module iasylum/siscweb
  (start-siscweb-service)

  (define start-siscweb-service
    (lambda* ((async: async #t))
             (define (set-jvm-system-property property-name property-value)
               (j "System.getProperties().setProperty(sjsppropsname, sjsppropsvalue);"
                  `((sjsppropsname ,(->jstring property-name)) (sjsppropsvalue ,(->jstring property-value)))))

             (set-jvm-system-property "sisc.heap" "/base/bedlam/sisc/sisc-1.16.6/sisc.shp")
             (add-jars-in-directory-to-classpath "/base/bedlam/siscweb/jetty-distribution-9.3.9.v20160517/lib")

             (let ((my-server
                    (j (quote-convert
"import java.io.File;
import java.lang.management.ManagementFactory;
import org.eclipse.jetty.jmx.MBeanContainer;
import org.eclipse.jetty.security.HashLoginService;
import org.eclipse.jetty.server.Server;
import org.eclipse.jetty.server.handler.AllowSymLinkAliasChecker;
import org.eclipse.jetty.webapp.WebAppContext;
// Create a basic jetty server object that will listen on port 8080.
// Note that if you set this to port 0 then a randomly available port
// will be assigned that you can either look in the logs for the port,
// or programmatically obtain it for use in test cases.
Server server = new Server(8080);

 // Setup JMX
MBeanContainer mbContainer = new MBeanContainer(
        ManagementFactory.getPlatformMBeanServer());
server.addBean(mbContainer);

// The WebAppContext is the entity that controls the environment in
// which a web application lives and breathes. In this example the
// context path is being set to '/' so it is suitable for serving root
// context requests and then we see it setting the location of the war.
// A whole host of other configurations are available, ranging from
// configuring to support annotation scanning in the webapp (through
// PlusConfiguration) to choosing where the webapp will unpack itself.
WebAppContext webapp = new WebAppContext('/base/bedlam/siscweb/web-app-context/','/');

webapp.addAliasCheck(new AllowSymLinkAliasChecker());

// A WebAppContext is a ContextHandler as well so it needs to be set to
// the server so it is aware of where to send the appropriate requests.
server.setHandler(webapp);

// Start things up!
server.start();
server;"))))
               (when (not async)
                 (j "// The use of server.join() the will make the current thread join and
                     // wait until the server is done executing.
                     // See http://docs.oracle.com/javase/7/docs/api/java/lang/Thread.html#join()
                     finserver.join();" `((finserver ,my-server)))))
             #t)))
