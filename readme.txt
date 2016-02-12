Bedlam is a set of Scheme libraries for SISC conveniently packaged together.

To use this code you should simply do (replace the directory entry below by the place where you decompressed bedlam):

   (begin (define iasylum-bedlam-location "/base/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")))

under SISC 1.16.6 or later (there is a version of it bundled in bedlam itself).

Another option is simply including jars/bedlam-bundle.jar and the other .jar files in lib in your project and using the BedlamBundleInit java class.

Finally, docker images are provided. See docker/ and docker-ubuntu/.

Why?

   This was developed during years and supports several production deployments of SISC. Sisc is a great Scheme implementation and having a big glob of wrappers and general scheme utilities to use in multiple projects has proved very useful.

What is supported?

   * XML through the sxml (http://okmij.org/ftp/Scheme/xml.html#SXML-spec) library ( some easy-to-grasp included in ./docs/sxml-tutorial.html ).

   * Basic job scheduling support using Quartz (http://www.quartz-scheduler.org/) to schedule Scheme Closures for execution using crontab-like notation.

   * jcode - an alternative FFI for Java that is  really easy to use.
Two examples (of use of jcode):
     http://github.com/igorhvr/bedlam/blob/master/iasylum/email.scm
     http://github.com/igorhvr/bedlam/blob/master/iasylum/excel.scm

   * Alex Shinn's: implementation of a pattern-matching library (see match.pdf inside for details on how to use it), irregex (http://synthcode.com/scheme/irregex/ ) excelent regular expressions & SRE library, and fmt (for formatting things to strings - beware that it as it stands fails for huge exact numbers).

   * SRFI-88 (keyword objects / http://srfi.schemers.org/srfi-88/srfi-88.html) support (reference implementation used - no self-evaluation property of keyword objects).

   * SRFI-89 (optional and named parameters / http://srfi.schemers.org/srfi-89/srfi-89.html) support.

   * A simple queue implementation (based on java.util.concurrent.ConcurrentLinkedQueue). Also experimental code to use the hornetq queuing service.

   * Basic support for i18n (retrieving messages from bundles).

   * Excel parsing and spreadsheet generation in Scheme (used Apache Poi - http://poi.apache.org/ - underneath) - including list->spreadsheet, for-each-excel-sheet-data and converting excel's Dates to Scheme dates.

   * Access database reading using the Jackcess library.

   * Misc utilities for dealing with jdbc in postgresql and java.(result-set->iterator; etc). MySql support is also included.

   * Clojure: embedded interpreter, utilities for easily running clojure code inside strings.

   * Javascript: utilities to run js code with embedded Javascript interpreter (Rhino); object Notation (http://www.json.org/) parser, generator and utilities: json-read, json-write, scheme->json, json->scheme.

   * Memoize functionality.

   * Simple logging facilities.

   * A packrat parser (ported from PLT Scheme).

   * Password generation module (ported from PLT Scheme).

   * Dorai Sitaram's Schelog embedding of Prolog in Scheme http://www.ccs.neu.edu/home/dorai/schelog/schelog.html

   * Aubrey Jaffer's Slib (http://people.csail.mit.edu/jaffer/SLIB) library which brings a LOT of functionality to scheme.

   * Olin Shivers' let-optionals (low-level / define-macro).

   * Sparse arrays and sparse vectors taken from Chicken Scheme (as-is).

   * Debugging facilities - you can stop in the middle of execution of a function and get hold of a REPL with local variables you specify (taken from a mailing list post).

   * Very easy to use JMX support (making creating of interfaces and all that mess unnecessary to expose beans).

   * A few other misc utilities (small things like a log where you are able to specify a base, a loop macro, each-for, a version of for-each with swapped arguments, and many others) and examples scattered.
