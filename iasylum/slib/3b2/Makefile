# Makefile for Scheme Library
# Author: Aubrey Jaffer
#
# This code is in the public domain.

SHELL = /bin/sh
intro:
	@echo
	@echo "Welcome to SLIB.  Read \"README\" and \"slib.info\" (or"
	@echo "\"slib.texi\") to learn how to install and use SLIB."
	@echo
	-make slib.info

VERSION = 3b2
RELEASE = 1

rpm_prefix=$(HOME)/rpmbuild/
prefix = /usr/local/
exec_prefix = $(prefix)
# directory where `make install' will put executable.
bindir = $(exec_prefix)bin/
libdir = $(exec_prefix)lib/
libslibdir = $(libdir)slib/
# directory where `make install' will put manual page.
man1dir = $(prefix)man/man1/
infodir = $(prefix)info/

PREVDOCS = prevdocs/

htmldir=../public_html/

RUNNABLE = scheme48
S48INIT = scheme48.init
S48LIB = $(libdir)$(RUNNABLE)/
S48SLIB = $(S48LIB)slib/
VM = scheme48vm
IMAGE48 = slib.image
INSTALL_DATA = install -c

$(DESTDIR)$(S48LIB)slibcat:
	touch $(DESTDIR)$(S48LIB)slibcat

catalogs:
	-if type scm; then scm -c "(require 'new-catalog)"; fi
	-if type guile; then guile -l guile.init -c\
	  "(use-modules (ice-9 slib)) (require 'new-catalog)"; fi
	-if type umb-scheme; then SCHEME_INIT=umbscheme.init;export SCHEME_INIT;\
	  echo "(require 'new-catalog)" | umb-scheme; fi
	-if type mzscheme; then\
	  SCHEME_LIBRARY_PATH=`pwd`/;export SCHEME_LIBRARY_PATH;\
	  cp mkpltcat.scm `mzscheme -mf mzscheme.init -e '(begin(display(implementation-vicinity))(exit))'`mkimpcat.scm;\
	  mzscheme -g -f mzscheme.init -e "(require 'new-catalog)" </dev/null; fi
	-if type scheme48; then make install48; fi

MKNMDB = (require 'color-database) (make-slib-color-name-db) (slib:exit)
clrnamdb: clrnamdb.scm
clrnamdb.scm:  mkclrnam.scm color.scm resenecolours.txt saturate.txt nbs-iscc.txt
	if type scm; then scm -e"$(MKNMDB)";\
	elif type guile; then guile -l guile.init -c\
	 "(use-modules (ice-9 slib)) $(MKNMDB)";\
	elif type slib48; then echo -e "$(MKNMDB)\n,exit" | slib48 -h 3000000;\
	elif type umb-scheme; then SCHEME_INIT=`pwd`/umbscheme.init;export SCHEME_INIT;\
	 echo "$(MKNMDB)" | umb-scheme;\
	elif type mzscheme; then SCHEME_LIBRARY_PATH=`pwd`/;export SCHEME_LIBRARY_PATH;\
	 echo "$(MKNMDB)" | mzscheme -f mzscheme.init;\
	fi

$(DESTDIR)$(S48SLIB)strport.scm:
	mkdir -p $(DESTDIR)$(S48SLIB)
	echo ";;; strport.scm  -*- scheme -*-">$(DESTDIR)$(S48SLIB)strport.scm
	echo ";@">>$(DESTDIR)$(S48SLIB)strport.scm
	echo "(define (call-with-output-string proc)">>$(DESTDIR)$(S48SLIB)strport.scm
	echo "  (let ((port (make-string-output-port)))">>$(DESTDIR)$(S48SLIB)strport.scm
	echo "    (proc port)">>$(DESTDIR)$(S48SLIB)strport.scm
	echo "    (string-output-port-output port)))">>$(DESTDIR)$(S48SLIB)strport.scm
	echo "(define (call-with-input-string string proc)">>$(DESTDIR)$(S48SLIB)strport.scm
	echo "  (proc (make-string-input-port string)))">>$(DESTDIR)$(S48SLIB)strport.scm

$(DESTDIR)$(S48SLIB)record.scm:
	mkdir -p $(DESTDIR)$(S48SLIB)
	echo ";;; record.scm  -*- scheme -*-">$(DESTDIR)$(S48SLIB)record.scm
	echo ";; This code is in the public domain">>$(DESTDIR)$(S48SLIB)record.scm
	echo ";@">>$(DESTDIR)$(S48SLIB)record.scm
	echo "(define make-record-type   make-record-type)">>$(DESTDIR)$(S48SLIB)record.scm
	echo "(define record-constructor">>$(DESTDIR)$(S48SLIB)record.scm
	echo "  (let ((constructor record-constructor))">>$(DESTDIR)$(S48SLIB)record.scm
	echo "    (lambda (rt . fields)">>$(DESTDIR)$(S48SLIB)record.scm
	echo "      (constructor rt (if (pair? fields)">>$(DESTDIR)$(S48SLIB)record.scm
	echo "                          (car fields)">>$(DESTDIR)$(S48SLIB)record.scm
	echo "                          (record-type-field-names rt))))))">>$(DESTDIR)$(S48SLIB)record.scm
	echo "(define record-predicate   record-predicate)">>$(DESTDIR)$(S48SLIB)record.scm
	echo "(define record-accessor    record-accessor)">>$(DESTDIR)$(S48SLIB)record.scm
	echo "(define record-modifier    record-modifier)">>$(DESTDIR)$(S48SLIB)record.scm

slib48:	$(IMAGE48)
$(IMAGE48):	Makefile $(S48INIT)
	S48_VERSION="`echo ,exit | $(RUNNABLE) | sed -n 's/Welcome to Scheme 48 //;s/ ([^)]*)[.]//;p;q'`";export S48_VERSION;\
	S48_VICINITY="$(DESTDIR)$(S48LIB)";export S48_VICINITY;\
	SCHEME_LIBRARY_PATH="`pwd`/";export SCHEME_LIBRARY_PATH;\
	$(RUNNABLE) < $(S48INIT)
install48:	$(IMAGE48) $(DESTDIR)$(S48SLIB)strport.scm $(DESTDIR)$(S48SLIB)record.scm
	$(INSTALL_DATA) $(IMAGE48) $(DESTDIR)$(S48LIB)
	(echo '#! /bin/sh';\
	 echo exec $(RUNNABLE) -i '$(DESTDIR)$(S48LIB)$(IMAGE48)' \"\$$\@\") \
	  > $(DESTDIR)$(bindir)slib48
	chmod +x $(DESTDIR)$(bindir)slib48

ffiles = format.scm printf.scm genwrite.scm pp.scm \
	ppfile.scm strcase.scm debug.scm trace.scm \
	strport.scm scanf.scm qp.scm break.scm stdio.scm \
	strsrch.scm prec.scm schmooz.scm defmacex.scm mbe.scm
lfiles = sort.scm comlist.scm logical.scm
revfiles = sc4opt.scm sc4sc3.scm sc2.scm mularg.scm mulapply.scm \
	trnscrpt.scm withfile.scm dynwind.scm promise.scm \
	values.scm eval.scm null.scm
afiles = charplot.scm root.scm cring.scm selfset.scm limit.scm \
	 timecore.scm psxtime.scm cltime.scm timezone.scm tzfile.scm \
	 math-real.scm
bfiles = fluidlet.scm object.scm recobj.scm yasyn.scm collect.scm collectx.scm
scfiles = r4rsyn.scm scmacro.scm synclo.scm synrul.scm synchk.scm \
	repl.scm macwork.scm mwexpand.scm mwdenote.scm mwsynrul.scm
scafiles = scainit.scm scaglob.scm scamacr.scm scaoutp.scm scaexpp.scm \
	structure.scm
srfiles = srfi-2.scm srfi-8.scm srfi-9.scm srfi-11.scm srfi-23.scm srfi-61.scm
efiles = record.scm dynamic.scm process.scm hash.scm \
	wttree.scm wttest.scm sierpinski.scm soundex.scm simetrix.scm
rfiles = rdms.scm alistab.scm paramlst.scm \
	batch.scm crc.scm dbrowse.scm getopt.scm dbinterp.scm \
	dbcom.scm dbsyn.scm
ciefiles = cie1931.xyz cie1964.xyz resenecolours.txt saturate.txt \
	nbs-iscc.txt ciesid65.dat ciesia.dat

txiscms =grapheps.scm glob.scm getparam.scm \
	vet.scm top-refs.scm hashtab.scm chap.scm comparse.scm\
	alist.scm ratize.scm modular.scm dirs.scm priorque.scm queue.scm\
	srfi.scm srfi-1.scm xml-parse.scm\
	pnm.scm http-cgi.scm htmlform.scm html4each.scm db2html.scm uri.scm\
	dft.scm solid.scm random.scm randinex.scm obj2str.scm ncbi-dna.scm\
	minimize.scm factor.scm determ.scm daylight.scm colornam.scm\
	mkclrnam.scm color.scm subarray.scm dbutil.scm array.scm transact.scm\
	arraymap.scm phil-spc.scm lineio.scm differ.scm cvs.scm tree.scm\
	coerce.scm byte.scm bytenumb.scm matfile.scm tsort.scm manifest.scm\
	peanosfc.scm linterp.scm math-integer.scm
txifiles =grapheps.txi glob.txi getparam.txi\
	vet.txi top-refs.txi hashtab.txi chap.txi comparse.txi\
	alist.txi ratize.txi modular.txi dirs.txi priorque.txi queue.txi\
	srfi.txi srfi-1.txi xml-parse.txi\
	pnm.txi http-cgi.txi htmlform.txi html4each.txi db2html.txi uri.txi\
	dft.txi solid.txi random.txi randinex.txi obj2str.txi ncbi-dna.txi\
	minimize.txi factor.txi determ.txi daylight.txi colornam.txi\
	mkclrnam.txi color.txi subarray.txi dbutil.txi array.txi transact.txi\
	arraymap.txi phil-spc.txi lineio.txi differ.txi cvs.txi tree.txi\
	coerce.txi byte.txi bytenumb.txi matfile.txi tsort.txi manifest.txi\
	peanosfc.txi linterp.txi math-integer.txi
#txifiles = `echo $(txiscms) | sed 's%.scm%.txi%g'`

texifiles = schmooz.texi indexes.texi object.texi format.texi limit.texi \
	 fdl.texi
docfiles = ANNOUNCE README COPYING FAQ slib.1 slib.info slib.texi version.txi\
	 ChangeLog $(texifiles) $(txifiles)
mkfiles = Makefile require.scm Template.scm mklibcat.scm mkpltcat.scm \
	syncase.sh Bev2slib.scm slib.spec slib.sh grapheps.ps slib.nsi
ifiles = bigloo.init chez.init elk.init macscheme.init mitscheme.init \
	scheme2c.init scheme48.init gambit.init t3.init vscm.init \
	scm.init scsh.init sisc.init pscheme.init STk.init kawa.init \
	RScheme.init mzscheme.init umbscheme.init guile.init jscheme.init
tfiles = macrotst.scm dwindtst.scm formatst.scm
sfiles = $(ffiles) $(lfiles) $(revfiles) $(afiles) $(scfiles) $(efiles) \
	$(rfiles) colorspc.scm $(scafiles) $(txiscms) $(srfiles)
allfiles = $(docfiles) $(mkfiles) $(ifiles) $(sfiles) $(tfiles) $(bfiles) \
	slib.doc $(ciefiles) clrnamdb.scm SLIB.ico

pinstall:	slib.1
	mkdir -p $(DESTDIR)$(man1dir)
	-cp slib.1 $(DESTDIR)$(man1dir)

install:	pinstall clrnamdb.scm
	mkdir -p $(DESTDIR)$(libslibdir)
	-cp  $(ifiles) $(sfiles) $(bfiles) $(mkfiles) clrnamdb.scm $(DESTDIR)$(libslibdir)
	mkdir -p $(DESTDIR)$(bindir)
	echo '#! /bin/sh'			 > $(DESTDIR)$(bindir)slib
	echo SCHEME_LIBRARY_PATH=$(libslibdir)  >> $(DESTDIR)$(bindir)slib
	echo S48_VICINITY=$(S48LIB)		>> $(DESTDIR)$(bindir)slib
	echo VERSION=$(VERSION)			>> $(DESTDIR)$(bindir)slib
	echo export SCHEME_LIBRARY_PATH S48_VICINITY >> $(DESTDIR)$(bindir)slib
	cat slib.sh				>> $(DESTDIR)$(bindir)slib
	chmod +x $(DESTDIR)$(bindir)slib

uninstall:
	-(cd $(DESTDIR)$(libslibdir); rm $(ifiles) $(sfiles) $(bfiles) $(mkfiles) clrnamdb.scm)
	-rm $(DESTDIR)$(bindir)slib
	-rm $(DESTDIR)$(man1dir)slib.1

slib.doc:	slib.1
	nroff -man $< | ul -tunknown >$@

slib.html:	slib.texi
	makeinfo --html --no-split --no-warn --force slib.texi

## to build a windows installer
## make sure makeinfo and NSIS are available on the commandline
w32install:	slib.html
	makensis slib.nsi

#### Stuff for maintaining SLIB below ####

ver = $(VERSION)
version.txi:	Makefile
	echo @set SLIBVERSION $(ver) > version.txi
	echo @set SLIBDATE `date +"%B %Y"` >> version.txi

scheme = scm

collect.sc:
	echo "(require 'macros-that-work)" > collect.sc
	echo "(require 'pprint-file)" >> collect.sc
	echo "(require 'yasos)" >> collect.sc
	echo "(pprint-filter-file \"collect.scm\" macwork:expand \"collectx.scm\")" >> collect.sc
	echo "(slib:exit #t)" >> collect.sc

collectx.scm: collect.scm macwork.scm collect.sc
	$(scheme) < collect.sc

$(txifiles):	$(txiscms) schmooz.scm
	$(scheme) -rschmooz -e'(schmooz "slib.texi")'

dvi:	slib.dvi
slib.dvi:	version.txi slib.texi $(txifiles) $(texifiles)
	texi2dvi -b -c slib.texi
xdvi:	slib.dvi
	xdvi -s 4 slib.dvi

pdf:	$(htmldir)slib.pdf
$(htmldir)slib.pdf:	version.txi slib.texi $(txifiles) $(texifiles)
#	dvipdf slib.dvi # doesn't have links!
	texi2pdf -b -c slib.texi
	mv slib.pdf $(htmldir)
xpdf:	$(htmldir)slib.pdf
	xpdf $(htmldir)slib.pdf

TEXI2HTML = /usr/local/bin/texi2html -split -verbose
slib_toc.html:	version.txi slib.texi $(txifiles) $(texifiles)
	${TEXI2HTML} slib.texi
html:	$(htmldir)slib_toc.html
$(htmldir)slib_toc.html:	slib_toc.html Makefile
	-rm -f slib_stoc.html
	if [ -f $(PREVDOCS)slib_toc.html ]; \
		then hitch $(PREVDOCS)slib_\*.html slib_\*.html $(htmldir); \
		else cp slib_*.html $(htmldir);fi

slib-$(VERSION).info:	version.txi slib.texi $(txifiles) $(texifiles)
	makeinfo slib.texi --no-warn --no-split -o slib.info
	mv slib.info slib-$(VERSION).info
slib.info:	slib-$(VERSION).info
	if [ -f $(PREVDOCS)slib.info ];\
		then infobar $(PREVDOCS)slib.info slib-$(VERSION).info slib.info;\
		else cp slib-$(VERSION).info slib.info;fi
info:	installinfo
installinfo:	$(DESTDIR)$(infodir)slib.info
$(DESTDIR)$(infodir)slib.info:	slib.info
	mkdir -p $(DESTDIR)$(infodir)
	cp -p slib.info $(DESTDIR)$(infodir)slib.info
	-install-info $(DESTDIR)$(infodir)slib.info $(DESTDIR)$(infodir)dir
	-rm $(DESTDIR)$(infodir)slib.info.gz
infoz:	installinfoz
installinfoz:	$(DESTDIR)$(infodir)slib.info.gz
$(DESTDIR)$(infodir)slib.info.gz:	$(DESTDIR)$(infodir)slib.info
	gzip -f $(DESTDIR)$(infodir)slib.info

docs:	$(DESTDIR)$(infodir)slib.info.gz $(htmldir)slib_toc.html slib.dvi \
	 $(htmldir)slib.pdf slib.doc
	xdvi -s 4 slib.dvi

makedev = make -f $(HOME)/makefile.dev
CHPAT=$(HOME)/bin/chpat
RSYNC=rsync -bav
UPLOADEE=swissnet_upload
dest = $(HOME)/dist/
DOSCM = /c/Voluntocracy/dist/

temp/slib:	$(allfiles)
	-rm -rf temp
	mkdir -p temp/slib
	ln  $(allfiles) temp/slib

infotemp/slib:	slib.info
	-rm -rf infotemp
	mkdir -p infotemp/slib
	ln slib.info slib.info-* infotemp/slib
#For change-barred HTML.
prevdocs:	$(PREVDOCS)slib_toc.html $(PREVDOCS)slib.info
$(PREVDOCS)slib_toc.html:
$(PREVDOCS)slib.info:	Makefile
	cd $(PREVDOCS); unzip -ao $(dest)slib*.zip
	rm $(PREVDOCS)slib/slib.info
	cd $(PREVDOCS)slib; make slib.info; make slib_toc.html
	cd $(PREVDOCS); mv -f slib/slib.info slib/*.html ./
	rm -rf $(PREVDOCS)slib
	-rm -f slib-$(VERSION).info

distinfo:	$(dest)slib.info.zip
$(dest)slib.info.zip:	infotemp/slib
	$(makedev) TEMP=infotemp/ DEST=$(dest) PROD=slib ver=.info zip
	rm -rf infotemp

release:	dist pdf tar.gz	# rpm
	cvs tag -F slib-$(VERSION)
	cp ANNOUNCE $(htmldir)SLIB_ANNOUNCE.txt
	cp COPYING $(htmldir)SLIB_COPYING.txt
	cp FAQ $(htmldir)SLIB.FAQ
	$(RSYNC) $(htmldir)SLIB.html $(htmldir)SLIB_ANNOUNCE.txt \
	 $(htmldir)SLIB_COPYING.txt $(UPLOADEE):public_html/
	$(RSYNC) $(dest)README $(dest)slib-$(VERSION).zip \
	 $(dest)slib-$(VERSION).tar.gz \
	 $(dest)slib-$(VERSION)-$(RELEASE).noarch.rpm \
	 $(dest)slib-$(VERSION)-$(RELEASE).src.rpm $(UPLOADEE):dist/
#	upload $(dest)README $(dest)slib-$(VERSION).zip ftp.gnu.org:gnu/jacal/

upzip:	$(HOME)/pub/slib.zip
	$(RSYNC) $(HOME)/pub/slib.zip $(UPLOADEE):pub/

dist:	$(dest)slib-$(VERSION).zip
$(dest)slib-$(VERSION).zip:	temp/slib
	$(makedev) DEST=$(dest) PROD=slib ver=-$(VERSION) zip

tar.gz:	$(dest)slib-$(VERSION).tar.gz
$(dest)slib-$(VERSION).tar.gz:	temp/slib
	$(makedev) DEST=$(dest) PROD=slib ver=-$(VERSION) tar.gz

rpm:	pubzip
#$(dest)slib-$(VERSION)-$(RELEASE).noarch.rpm:	$(dest)slib-$(VERSION).zip
	cp $(HOME)/pub/slib.zip $(rpm_prefix)SOURCES/slib-$(VERSION).zip
	rpmbuild -ba slib.spec	# --clean
	rm $(rpm_prefix)SOURCES/slib-$(VERSION).zip
	mv $(rpm_prefix)RPMS/noarch/slib-$(VERSION)-$(RELEASE).noarch.rpm \
	   $(rpm_prefix)SRPMS/slib-$(VERSION)-$(RELEASE).src.rpm $(dest)

shar:	slib.shar
slib.shar:	temp/slib
	$(makedev) PROD=slib shar
dclshar:	slib.com
com:	slib.com
slib.com:	temp/slib
	$(makedev) PROD=slib com
zip:	slib.zip
slib.zip:	temp/slib
	$(makedev) DEST=../ PROD=slib zip
doszip:	$(DOSCM)slib-$(VERSION).zip
$(DOSCM)slib-$(VERSION).zip:	temp/slib slib.html
	$(makedev) DEST=$(DOSCM) PROD=slib ver=-$(VERSION) zip
	-cd ..; zip -9ur $(DOSCM)slib-$(VERSION).zip slib/slib.html
	zip -d $(DOSCM)slib-$(VERSION).zip slib/slib.info
pubzip:	temp/slib
	$(makedev) DEST=$(HOME)/pub/ PROD=slib zip

diffs:	pubdiffs
pubdiffs:	temp/slib
	$(makedev) DEST=$(HOME)/pub/ PROD=slib pubdiffs
distdiffs:	temp/slib
	$(makedev) DEST=$(dest) PROD=slib ver=$(ver) distdiffs
announcediffs:	temp/slib
	$(makedev) DEST=$(dest) PROD=slib ver=-$(VERSION) announcediffs

psdfiles=COPYING.psd README.psd cmuscheme.el comint.el instrum.scm pexpr.scm \
	primitives.scm psd-slib.scm psd.el read.scm runtime.scm version.scm
psdocfiles=article.bbl article.tex manual.bbl manual.tex quick-intro.tex

psdtemp/slib:
	-rm -rf psdtemp
	mkdir -p psdtemp/slib/psd
	cd psd; ln $(psdfiles) ../psdtemp/slib/psd
	mkdir -p psdtemp/slib/psd/doc
	cd psd/doc; ln $(psdocfiles) ../../psdtemp/slib/psd/doc

psdist:	$(dest)slib-psd.tar.gz
$(dest)slib-psd.tar.gz:	psdtemp/slib
	$(makedev) DEST=$(dest) PROD=slib ver=-psd tar.gz TEMP=psdtemp/

CITERS = FAQ README ANNOUNCE \
	../dist/README \
	../jacal/ANNOUNCE ../jacal/jacal.texi \
	../scm/ANNOUNCE ../scm/scm.texi \
	../wb/ANNOUNCE ../wb/README \
	../synch/ANNOUNCE \
	$(DOSCM)unzipall.bat $(DOSCM)buildall \
	$(htmldir)JACAL.html \
	$(htmldir)README.html \
	$(htmldir)SCM.html \
	$(htmldir)SIMSYNCH.html \
	$(htmldir)FreeSnell/ANNOUNCE $(htmldir)FreeSnell/index.html
CITES =  require.scm Makefile slib.spec scheme48.init \
	$(htmldir)SLIB.html slib.nsi ../scm/scm.nsi

new:
	$(CHPAT) slib-$(VERSION) slib-$(ver) $(CITERS)
	$(CHPAT) $(VERSION) $(ver) $(CITES)
	echo `date -I` \ Aubrey Jaffer \ \<`whoami`@`hostname`\>> change
	echo>> change
	echo \	\* require.scm \(*slib-version*\): Bumped from $(VERSION) to $(ver).>>change
	echo>> change
	cat ChangeLog >> change
	mv -f change ChangeLog
	cvs commit -lm '(*slib-version*): Bumped from $(VERSION) to $(ver).'
	cvs tag -F slib-$(ver)

tagfiles = README version.txi slib.texi $(texifiles) \
	$(mkfiles) $(sfiles) $(bfiles) $(tfiles) $(ifiles)
# README and $(ifiles) cause semgentation faults in ETAGS for Emacs version 19.
tags:	$(tagfiles)
	etags $(tagfiles)
rights:
	$(scheme) -ladmin -e"(admin:check-all)" $(sfiles) $(tfiles) \
		$(bfiles) $(ifiles)
report:
	$(scheme) -e"(slib:report #t)"
clean:
	-rm -f *~ *.bak *.orig *.rej core a.out *.o \#*
	-rm -rf *temp
distclean:	realclean
realclean:
	-rm -f *~ *.bak *.orig *.rej TAGS core a.out *.o \#*
	-rm -f slib.info* slib.?? slib.html
	-rm -rf *temp SLIB-*.exe
realempty:	temp/slib
	-rm -f $(allfiles)
