(require-extension (lib iasylum/jcode))

(module iasylum/siscweb
  (start-siscweb-service)

  (define (start-siscweb-service)
    (define (set-jvm-system-property property-name property-value)
      (j "System.getProperties().setProperty(sjsppropsname, sjsppropsvalue);"
         `((sjsppropsname ,(->jstring property-name)) (sjsppropsvalue ,(->jstring property-value)))))
    
    (set-jvm-system-property "catalina.base" "/base/bedlam/siscweb/bedlam-apache-tomcat-6.0.18")
    (set-jvm-system-property "catalina.home" "/base/bedlam/siscweb/bedlam-apache-tomcat-6.0.18")
    (set-jvm-system-property "java.io.tmpdir" "/base/bedlam/siscweb/bedlam-apache-tomcat-6.0.18/temp")
    
    (set-jvm-system-property "common.loader"
                             "/base/bedlam/siscweb/bedlam-apache-tomcat-6.0.18/lib,/base/bedlam/siscweb/bedlam-apache-tomcat-6.0.18/lib/*.jar,/base/bedlam/jars/aws-java-sdk/*.jar,/base/bedlam/jars/bsh/*.jar,/base/bedlam/jars/clojure/*.jar,/base/bedlam/jars/excel/*.jar,/base/bedlam/jars/hornetq/*.jar,/base/bedlam/jars/jackson/*.jar,/base/bedlam/jars/javascript/*.jar,/base/bedlam/jars/javassist/*.jar,/base/bedlam/jars/jdbc/*.jar,/base/bedlam/jars/joda/*.jar,/base/bedlam/jars/log/*.jar,/base/bedlam/jars/microsoft_access/*.jar,/base/bedlam/jars/net/*.jar,/base/bedlam/jars/sisc/*.jar,/base/bedlam/jars/siscweb/*.jar,/base/bedlam/jars/tomcat/*.jar,/base/bedlam/jars/u/*.jar/*.jar,/base/bedlam/jars/bedlam-bundle.jar,/base/bedlam/jars/u/java-base.jar")
    
    (extend-classpath "/base/bedlam/siscweb/bedlam-apache-tomcat-6.0.18/webapps/ROOT/WEB-INF/classes/")
    
    (extend-classpath "/base/bedlam/siscweb/bedlam-apache-tomcat-6.0.18/webapps/ROOT/WEB-INF/scm/")
    
    (extend-classpath "/base/bedlam/jars/u/java-base.jar")
    
    (extend-classpath "/base/bedlam/jars/u/bsf.jar")
    
    (extend-classpath "/base/bedlam/siscweb/bedlam-apache-tomcat-6.0.18/bin/bootstrap.jar")
    (extend-classpath "/base/bedlam/siscweb/bedlam-apache-tomcat-6.0.18/bin/commons-daemon.jar")
    (extend-classpath "/base/bedlam/siscweb/bedlam-apache-tomcat-6.0.18/bin/tomcat-juli.jar")
    (extend-classpath "/base/bedlam/siscweb/bedlam-apache-tomcat-6.0.18/lib/annotations-api.jar")
    (extend-classpath "/base/bedlam/siscweb/bedlam-apache-tomcat-6.0.18/lib/catalina-ant.jar")
    (extend-classpath "/base/bedlam/siscweb/bedlam-apache-tomcat-6.0.18/lib/catalina-ha.jar")
    (extend-classpath "/base/bedlam/siscweb/bedlam-apache-tomcat-6.0.18/lib/catalina.jar")
    (extend-classpath "/base/bedlam/siscweb/bedlam-apache-tomcat-6.0.18/lib/catalina-tribes.jar")
    (extend-classpath "/base/bedlam/siscweb/bedlam-apache-tomcat-6.0.18/lib/el-api.jar")
    (extend-classpath "/base/bedlam/siscweb/bedlam-apache-tomcat-6.0.18/lib/jasper-el.jar")
    (extend-classpath "/base/bedlam/siscweb/bedlam-apache-tomcat-6.0.18/lib/jasper.jar")
    (extend-classpath "/base/bedlam/siscweb/bedlam-apache-tomcat-6.0.18/lib/jasper-jdt.jar")
    (extend-classpath "/base/bedlam/siscweb/bedlam-apache-tomcat-6.0.18/lib/jsp-api.jar")
    (extend-classpath "/base/bedlam/siscweb/bedlam-apache-tomcat-6.0.18/lib/servlet-api.jar")
    (extend-classpath "/base/bedlam/siscweb/bedlam-apache-tomcat-6.0.18/lib/tomcat-coyote.jar")
    (extend-classpath "/base/bedlam/siscweb/bedlam-apache-tomcat-6.0.18/lib/tomcat-dbcp.jar")
    
    (j "org.apache.catalina.startup.Bootstrap.main(new String[]{\"start\"});")))
    
