(use eggdoc)

(define license
"Copyright (c) 2004, 2005 Tony Garnock-Jones <tonyg@kcbbs.gen.nz>
Copyright (c) 2005 LShift Ltd. <query@lshift.net>

Permission is hereby granted, free of charge, to any person
obtaining a copy of this software and associated documentation
files (the \"Software\"), to deal in the Software without
restriction, including without limitation the rights to use, copy,
modify, merge, publish, distribute, sublicense, and/or sell copies
of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.")

(define doc
  `((eggdoc:begin
     (name "json")
     (description (p "A JSON library"))
     
     (author "Tony Garnock-Jones")

     (history
      (version "1.2" "handling of unicode escapes [by Dan Muresan]")
      (version "1.1" "Bugfix to handle null")
      (version "1.0" "Initial release"))

     (requires 
      (url "syntax-case.html" "syntax-case")
      (url "packrat.html" "packrat"))

     (usage)
     (download "json.egg")

     (documentation
      (p "This extension implements a parser and generator for the " (url "http://www.json.org" "JSON")
	 " data interchange format.")
      (group
       (procedure 
	"(json-write OBJECT [PORT])"
	(p "Writes the Scheme data object " (tt "OBJECT") " in JSON format to " (tt "PORT") ", which defaults"
	   " to the value of " (tt "(current-output-port)") ".")
	(p "Scheme data is mapped to JSON format in the following way:"
	   (table 
	    (@ (border "0"))
	    (tr (th (@ (align "left")) "Scheme") (th (@ (align "left")) "JSON"))
	    (tr (td "Hash-table") (td "Structure"))
	    (tr (td "Vector") (td "Structure (assuming vector consists of " (tt "(SYMBOL . DATA)") " pairs"))
	    (tr (td "List") (td "Array"))
	    (tr (td "Number") (td "Number"))
	    (tr (td "Void") (td "Null"))
	    (tr (td "String or symbol") (td "String")) ) ) )
       (procedure
	"(json-read [PORT])"
	(p "Reads data in JSON format from " (tt "PORT") ", which defaults to the value of "
	   (tt "(current-input-port)") ".") ) ) )

     (section "License" (pre ,license)))))

(eggdoc->html doc)

