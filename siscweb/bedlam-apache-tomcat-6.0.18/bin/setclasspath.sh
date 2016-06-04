#!/bin/sh

# Licensed to the Apache Software Foundation (ASF) under one or more
# contributor license agreements.  See the NOTICE file distributed with
# this work for additional information regarding copyright ownership.
# The ASF licenses this file to You under the Apache License, Version 2.0
# (the "License"); you may not use this file except in compliance with
# the License.  You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# -----------------------------------------------------------------------------
#  Set CLASSPATH and Java options
#
#  $Id: setclasspath.sh 589060 2007-10-27 08:19:24Z jfclere $
# -----------------------------------------------------------------------------

# First clear out the user classpath
export CLASSPATH=/base/bedlam/jars/javassist/javassist-rel_3_19_0_ga/javassist.jar:/base/bedlam/jars/javassist/javassist-rel_3_19_0_ga/javassist-src.jar:/base/bedlam/jars/javassist/javassist-rel_3_19_0_ga/.gitignore:/base/bedlam/jars/net/fluent-hc-4.3.4.jar:/base/bedlam/jars/net/httpmime-4.3.4.jar:/base/bedlam/jars/net/httpclient-cache-4.3.6.jar:/base/bedlam/jars/net/httpcore-4.3.3.jar:/base/bedlam/jars/net/commons-codec-1.6.jar:/base/bedlam/jars/net/httpclient-4.3.6.jar:/base/bedlam/jars/siscweb/siscweb-sxml.jar:/base/bedlam/jars/siscweb/siscweb.jar:/base/bedlam/jars/siscweb/siscweb-sql.jar:/base/bedlam/jars/u/incanter-1.5.6-SNAPSHOT-standalone-2_of_2.jar:/base/bedlam/jars/u/uuid-3.3-sources.jar:/base/bedlam/jars/u/scrypt-1.4.0.jar:/base/bedlam/jars/u/jdbc-postgresql.jar:/base/bedlam/jars/u/junrar-0.7-SNAPSHOT.jar:/base/bedlam/jars/u/json-io-2.7.2-SNAPSHOT-everything.jar:/base/bedlam/jars/u/junrar-0.7-SNAPSHOT-src.jar:/base/bedlam/jars/u/mail.jar:/base/bedlam/jars/u/js-engine.jar:/base/bedlam/jars/u/commons-fileupload-1.2.1.jar:/base/bedlam/jars/u/m.jar:/base/bedlam/jars/u/bsf.jar:/base/bedlam/jars/u/quartz-all-1.7.3.jar:/base/bedlam/jars/u/cron4j-2.2.5.jar:/base/bedlam/jars/u/commons-logging-1.1.3.jar:/base/bedlam/jars/u/lzstring4j.jar:/base/bedlam/jars/u/activation-1.1.jar:/base/bedlam/jars/u/commons-io-2.0.1.jar:/base/bedlam/jars/u/js.jar:/base/bedlam/jars/u/guava-15.0.jar:/base/bedlam/jars/u/ssax-sxml.jar:/base/bedlam/jars/u/uuid-3.3.jar:/base/bedlam/jars/u/commons-email-1.2.jar:/base/bedlam/jars/u/java-base.jar:/base/bedlam/jars/u/junit-3.8.1.jar:/base/bedlam/jars/u/clj-pdf-1.11.21-standalone.jar:/base/bedlam/jars/u/incanter-1.5.6-SNAPSHOT-standalone-1_of_2.jar:/base/bedlam/jars/sisc/sisc.shp:/base/bedlam/jars/sisc/sisc-opt.jar:/base/bedlam/jars/sisc/sisc.jar:/base/bedlam/jars/sisc/sisc-heap.jar:/base/bedlam/jars/sisc/sisc-lib.jar:/base/bedlam/jars/tomcat/jsp-api.jar:/base/bedlam/jars/tomcat/catalina-tribes.jar:/base/bedlam/jars/tomcat/tomcat-juli.jar:/base/bedlam/jars/tomcat/jasper.jar:/base/bedlam/jars/tomcat/jasper-el.jar:/base/bedlam/jars/tomcat/catalina-ant.jar:/base/bedlam/jars/tomcat/tomcat-i18n-ja.jar:/base/bedlam/jars/tomcat/bootstrap.jar:/base/bedlam/jars/tomcat/tomcat-i18n-fr.jar:/base/bedlam/jars/tomcat/commons-daemon.jar:/base/bedlam/jars/tomcat/standard.jar:/base/bedlam/jars/tomcat/tomcat-coyote.jar:/base/bedlam/jars/tomcat/jstl.jar:/base/bedlam/jars/tomcat/catalina-ha.jar:/base/bedlam/jars/tomcat/el-api.jar:/base/bedlam/jars/tomcat/tomcat-dbcp.jar:/base/bedlam/jars/tomcat/tomcat-i18n-es.jar:/base/bedlam/jars/tomcat/servlet-api.jar:/base/bedlam/jars/tomcat/catalina.jar:/base/bedlam/jars/tomcat/jasper-jdt.jar:/base/bedlam/jars/tomcat/annotations-api.jar:/base/bedlam/jars/bsh/bsh-2.2.0.jar:/base/bedlam/jars/jdbc/pool/HikariCP-2.3.2.pom:/base/bedlam/jars/jdbc/pool/HikariCP-2.3.2.jar:/base/bedlam/jars/jdbc/pool/HikariCP-2.3.2-javadoc.jar:/base/bedlam/jars/jdbc/pool/HikariCP-2.3.2-sources.jar:/base/bedlam/jars/jdbc/pool/HikariCP.git.zip:/base/bedlam/jars/jdbc/mysql-connector-java-5.1.24-bin_and_src.jar:/base/bedlam/jars/jdbc/jtds-1.2.5.jar:/base/bedlam/jars/clojure/clojure-1.5.1-sources.jar:/base/bedlam/jars/clojure/tools.nrepl-0.2.10.jar:/base/bedlam/jars/clojure/clojure-1.5.1.jar:/base/bedlam/jars/aws-java-sdk/javax-mail-1.4.6/LICENSE.txt:/base/bedlam/jars/aws-java-sdk/javax-mail-1.4.6/javax.mail-api-1.4.6.jar:/base/bedlam/jars/aws-java-sdk/spring-3.0/LICENSE.txt:/base/bedlam/jars/aws-java-sdk/spring-3.0/spring-beans-3.0.7.jar:/base/bedlam/jars/aws-java-sdk/spring-3.0/spring-test-3.0.7.RELEASE.jar:/base/bedlam/jars/aws-java-sdk/spring-3.0/spring-core-3.0.7.jar:/base/bedlam/jars/aws-java-sdk/spring-3.0/spring-context-3.0.7.jar:/base/bedlam/jars/aws-java-sdk/aws-java-sdk-1.10.44-sources.jar:/base/bedlam/jars/aws-java-sdk/aws-java-sdk-1.10.44-2_of_2.jar:/base/bedlam/jars/aws-java-sdk/aspectjrt-1.8.2.jar:/base/bedlam/jars/aws-java-sdk/aws-java-sdk-flow-build-tools-1.10.44.jar:/base/bedlam/jars/aws-java-sdk/aspectjweaver.jar:/base/bedlam/jars/aws-java-sdk/aws-java-sdk-1.10.44-1_of_2.jar:/base/bedlam/jars/aws-java-sdk/freemarker-2.3.9.jar:/base/bedlam/jars/aws-java-sdk/aws-java-sdk-1.10.44-javadoc.jar:/base/bedlam/jars/excel/poi-examples-3.9-20121203.jar:/base/bedlam/jars/excel/poi-scratchpad-3.9-20121203.jar:/base/bedlam/jars/excel/poi-excelant-3.9-20121203.jar:/base/bedlam/jars/excel/poi-ooxml-schemas-3.9-20121203.jar:/base/bedlam/jars/excel/poi-ooxml-3.9-20121203.jar:/base/bedlam/jars/excel/poi-3.9-20121203.jar:/base/bedlam/jars/jackson/jackson-core-2.4.0.jar:/base/bedlam/jars/jackson/jackson-annotations-2.4.0.jar:/base/bedlam/jars/jackson/jackson-datatype-joda-2.2.2.jar:/base/bedlam/jars/jackson/jackson-mapper-asl-1.9.13.jar:/base/bedlam/jars/jackson/jackson-core-asl-1.9.13.jar:/base/bedlam/jars/jackson/jackson-databind-2.4.0.jar:/base/bedlam/jars/hornetq/hornetq-commons-2.3.17.Final.jar:/base/bedlam/jars/hornetq/hornetq-server-2.3.17.Final.jar:/base/bedlam/jars/hornetq/hornetq-core-client-2.3.17.Final.jar:/base/bedlam/jars/hornetq/hornetq-journal-2.3.17.Final.jar:/base/bedlam/jars/javascript/j2v8_linux_x86_64-3.0.2.jar:/base/bedlam/jars/javascript/j2v8_linux_x86_64-3.0.2-javadoc.jar:/base/bedlam/jars/javascript/j2v8_linux_x86_64-3.0.2-sources.jar:/base/bedlam/jars/bedlam-bundle.jar:/base/bedlam/jars/log/jul-to-slf4j-1.7.5.jar:/base/bedlam/jars/log/log4j-slf4j-impl-2.0-rc2.jar:/base/bedlam/jars/log/slf4j-api-1.7.5.jar:/base/bedlam/jars/log/logback-core-1.0.13.jar:/base/bedlam/jars/log/log4j-core-2.0-rc2.jar:/base/bedlam/jars/log/log4j-1.2-api-2.0-rc2.jar:/base/bedlam/jars/log/log4j-api-2.0-rc2.jar:/base/bedlam/jars/microsoft_access/commons-lang-2.5.jar:/base/bedlam/jars/microsoft_access/jackcess-1.2.12.jar:/base/bedlam/jars/microsoft_access/commons-lang-2.5-sources.jar:/base/bedlam/jars/microsoft_access/commons-lang-2.5-javadoc.jar:/base/bedlam/jars/microsoft_access/jackcess-1.2.12-sources.jar:/base/bedlam/jars/joda/joda-time-2.9-sources.jar:/base/bedlam/jars/joda/joda-time-2.9.jar

# Make sure prerequisite environment variables are set
if [ -z "$JAVA_HOME" -a -z "$JRE_HOME" ]; then
  # Bugzilla 37284 (reviewed).
  if $darwin; then
    if [ -d "/System/Library/Frameworks/JavaVM.framework/Versions/CurrentJDK/Home" ]; then
      export JAVA_HOME="/System/Library/Frameworks/JavaVM.framework/Versions/CurrentJDK/Home"
    fi
  else
    JAVA_PATH=`which java 2>/dev/null`
    if [ "x$JAVA_PATH" != "x" ]; then
      JAVA_PATH=`dirname $JAVA_PATH 2>/dev/null`
      JRE_HOME=`dirname $JAVA_PATH 2>/dev/null`
    fi
    if [ "x$JRE_HOME" = "x" ]; then
      # XXX: Should we try other locations?
      if [ -x /usr/bin/java ]; then
        JRE_HOME=/usr
      fi
    fi
  fi
  if [ -z "$JAVA_HOME" -a -z "$JRE_HOME" ]; then
    echo "Neither the JAVA_HOME nor the JRE_HOME environment variable is defined"
    echo "At least one of these environment variable is needed to run this program"
    exit 1
  fi
fi
if [ -z "$JAVA_HOME" -a "$1" = "debug" ]; then
  echo "JAVA_HOME should point to a JDK in order to run in debug mode."
  exit 1
fi
if [ -z "$JRE_HOME" ]; then
  JRE_HOME="$JAVA_HOME"
fi

# If we're running under jdb, we need a full jdk.
if [ "$1" = "debug" -o "$1" = "javac" ] ; then
  if [ "$os400" = "true" ]; then
    if [ ! -x "$JAVA_HOME"/bin/java -o ! -x "$JAVA_HOME"/bin/javac ]; then
      echo "The JAVA_HOME environment variable is not defined correctly"
      echo "This environment variable is needed to run this program"
      echo "NB: JAVA_HOME should point to a JDK not a JRE"
      exit 1
    fi
  else
    if [ ! -x "$JAVA_HOME"/bin/java -o ! -x "$JAVA_HOME"/bin/jdb -o ! -x "$JAVA_HOME"/bin/javac ]; then
      echo "The JAVA_HOME environment variable is not defined correctly"
      echo "This environment variable is needed to run this program"
      echo "NB: JAVA_HOME should point to a JDK not a JRE"
      exit 1
    fi
  fi
fi
if [ -z "$BASEDIR" ]; then
  echo "The BASEDIR environment variable is not defined"
  echo "This environment variable is needed to run this program"
  exit 1
fi
if [ ! -x "$BASEDIR"/bin/setclasspath.sh ]; then
  if $os400; then
    # -x will Only work on the os400 if the files are:
    # 1. owned by the user
    # 2. owned by the PRIMARY group of the user
    # this will not work if the user belongs in secondary groups
    eval
  else
    echo "The BASEDIR environment variable is not defined correctly"
    echo "This environment variable is needed to run this program"
    exit 1
  fi
fi

# Set the default -Djava.endorsed.dirs argument
JAVA_ENDORSED_DIRS="$BASEDIR"/endorsed

# Set standard CLASSPATH
if [ "$1" = "javac" ] ; then
  if [ ! -f "$JAVA_HOME"/lib/tools.jar ]; then
    echo "Can't find tools.jar in JAVA_HOME"
    echo "Need a JDK to run javac"
    exit 1
  fi
fi
if [ "$1" = "debug" -o "$1" = "javac" ] ; then
  if [ -f "$JAVA_HOME"/lib/tools.jar ]; then
    CLASSPATH="$JAVA_HOME"/lib/tools.jar
  fi
fi

# OSX hack to CLASSPATH
JIKESPATH=
if [ `uname -s` = "Darwin" ]; then
  OSXHACK="/System/Library/Frameworks/JavaVM.framework/Versions/CurrentJDK/Classes"
  if [ -d "$OSXHACK" ]; then
    for i in "$OSXHACK"/*.jar; do
      JIKESPATH="$JIKESPATH":"$i"
    done
  fi
fi

# Set standard commands for invoking Java.
  _RUNJAVA="$JRE_HOME"/bin/java
if [ "$os400" != "true" ]; then
  _RUNJDB="$JAVA_HOME"/bin/jdb
fi
_RUNJAVAC="$JAVA_HOME"/bin/javac
