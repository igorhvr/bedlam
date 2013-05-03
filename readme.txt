Bedlam is a set of Scheme libraries for SISC conveniently packaged together.

To use this code you should simply do (replace the directory entry below by the place where you uncompressed bedlam):

   (begin (define iasylum-bedlam-location "/home/igorhvr/idm/bedlam/") (load (string-append iasylum-bedlam-location "iasylum/init.scm")))

under SISC 1.16.6 or later.

Another option is simply including jars/bedlam-bundle.jar and the other .jar files in lib in your project and using the BedlamBundleInit java class.

