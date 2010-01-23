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
     (name "packrat")
     (description (p "A packrat parsing library"))
     
     (author "Tony Garnock-Jones")

     (history
      (version "1.2" "Ported to CHICKEN 4")
      (version "1.1" "Added use of srfi-1")
      (version "1.0" "Initial release"))

     (usage)

     (documentation
      (p "Packrat parsing is a memoizing, backtracking recursive-descent parsing technique "
	 "that runs in time and space linear in the size of the input text. "
	 "The technique was originally discovered by Alexander Birman in 1970 "
	 ", and Bryan Ford took up the idea for his master's thesis in 2002.")
      (p "For detailed information on the technique, please see Bryan Ford's web "
	 "page at " (url "http://pdos.csail.mit.edu/~baford/packrat/" "http://pdos.csail.mit.edu/~baford/packrat/")
	 ".")
      (p "A reference manual can be found here: " (url "http://www.lshift.net/~tonyg/packrat.pdf" "packrat.pdf") ".") 
      (p "To use the packrat API, import the " (tt "packrat") " module.") )

     (section "License" (pre ,license)))))

(eggdoc->html doc)

