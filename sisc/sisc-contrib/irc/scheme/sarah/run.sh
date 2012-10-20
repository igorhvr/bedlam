PATH=/usr/local/java/bin:$PATH
CLASSPATH="libs/pircbot.jar:libs/sisc-pirc.jar:libs/postgresql.jar:.:$CLASSPATH"
SISC_HOME=sisc
JAVAOPT="-Dsisc.permitInterrupts=true -Dsisc.emitAnnotations=true -Dsisc.emitDebuggingSymbols=true"
export CLASSPATH SISC_HOME JAVAOPT PATH
cd anna
./server.sh &
ANNAPID=$!
echo $ANNAPID
cd ..

sisc/sisc ./config.scm ./libs.scm ./sarahbot.scm
kill $ANNAPID