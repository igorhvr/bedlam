Each of the sub-directories of contrib contains a contributed SISC module.

Modules typically have the following directory structure

/<modulename>

/<modulename>/build.xml
  Ant project file.
  Modules should locate the SISC distribution via the environment
  variable SISC_HOME or the sisc.home ant property.

/<modulename>/java/..
  Java source files; the sub-directories should correspond to the
  fully qualified package names. Typically this is
  sisc.contrib.<modulename>, resulting in files residing in
  /<modulename>/java/sisc/contrib/<modulename>/

/<modulename>/scheme/..
  Scheme source files

/<modulename>/lib/..
  Any required jars

/<modulename>/docs/..
  Documentation

