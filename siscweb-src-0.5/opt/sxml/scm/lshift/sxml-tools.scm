;;; Copyright (c) 2005, 2006 LShift Ltd. <query@lshift.net>
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining
;;; a copy of this software and associated documentation files (the
;;; "Software"), to deal in the Software without restriction, including
;;; without limitation the rights to use, copy, modify, merge, publish,
;;; distribute, sublicense, and/or sell copies of the Software, and to
;;; permit persons to whom the Software is furnished to do so, subject to
;;; the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;; Modules def for our SXML tools

(require-library 'sisc/libs/srfi/srfi-2) ; and-let*; an and with local bindings, a guarded let* special form
(require-library 'sisc/libs/srfi/srfi-13) ; string library

(require-library 'sxpath/sxpath)

(require-library 'lshift/error)


(module lshift/sxml-tools
    (current-sxml-node
     current-namespaces-for-xpath
     select
     value-of
     $
     with-namespaces ;; macro
     with-sxml-path ;; macro
     with-sxml ;; macro
     split-qname
     join-qname
     xxexpr-child-case
     xxexpr-edge-use-sxml-attributes
     make-xxexpr-edge
     expand-prefixes
     visit-xxexpr
     tag-equals-pred
     localname-equals-pred
     or-pred
     ge-rename-visitor
     ge-rename-visitor*
     replace-content-visitor
     replace-content-with-value-visitor
     prepend-child-to-edge
     edge-body
     )

  (import srfi-13)
  (import sxpath)
  (import srfi-2)
  (import lshift/error)

  ;; Get values from IDs, and that sort of thing.
  ;; Also, a parameter-based implementation of some of the core XSLT ideas.

  ;; --------------------------------------------------

  ;; current-sxml-node :: (parameter XXEXPR)
  (define current-sxml-node (make-parameter '()))

  ;; current-namespaces-for-xpath :: (parameter (alist symbol-or-false string-or-false))
  (define current-namespaces-for-xpath (make-parameter '()))

  ;; Terms "node", "edge" are defined as for xxexpr.ss:
  ;;
  ;; XXEXPR     :== node
  ;; node       :== (child . node) | ()
  ;; child      :== edge | atom | special
  ;; edge       :== (tag . node) | (tag ((attr atom) ...) . node)
  ;; atom       :== <non-list>

  ;; The following procedures use the parameters above as implicit
  ;; arguments.  The normal pattern is to use
  ;; with-sxml to introduce the node to work on, with-sxml-path to temporarily
  ;; work with a subnode, then $ to get the values of subnodes.  For example:
  ;; (with-sxml document
  ;;   (with-sxml-path "/html/head/*"
  ;;     ($ "title")))
  ;;
  ;; Note that these all deal with XXEXPR nodes, so you may need to use list and car
  ;; to deal with edges.

  ;; WARNING: we're not dealing with attributes in these utilities at
  ;; all well!


  ;; select :: string -> XXEXPR
  ;; (based on implicit values of current-sxml-node and current-namespaces-for-xpath)
  (define (select xpath)
    ((SXPATH:sxpath xpath (current-namespaces-for-xpath)) (cons '*top* (current-sxml-node))))

  ;; Similar to XSLT value-of, but not quite: XSLT's value-of needs an
  ;; xpath expression, since it can't deal with node-sets as such, but
  ;; we have no such limitation in Scheme, so we've separated the
  ;; selection stage from the traversal stage. See the definition of ($)
  ;; below.

  ;; value-of :: XXEXPR -> string
  (define (value-of nodeset)
    (string-concatenate (reverse (let walk ((node nodeset)
					    (acc '()))
				   (cond
				    ((pair? node) (walk (cdr node) (walk (car node) acc)))
				    ((string? node) (cons node acc))
				    (else acc))))))

  ;; $ :: string (as xpath expression) -> string (as CDATA)
  (define ($ xpath)
    (value-of (select xpath)))

  ;; (with-namespaces (list-of (list string-or-false string-or-false))
  ;;   body ...)
  ;; a value of #f on either side of an alist entry means 'default namespace'.
  (define-syntax with-namespaces
    (syntax-rules ()
      ((_ ((prefix-expr uri-expr) ...) body ...)
       (parameterize ((current-namespaces-for-xpath (append (list (cons
								   (and-let*
								       ((prefix prefix-expr))
								     (string->symbol prefix))
								   uri-expr) ...)
							    (current-namespaces-for-xpath))))
	 body ...))))

  (define-syntax with-sxml
    (syntax-rules ()
      ((_ node-producer body ...)
       (parameterize ((current-sxml-node node-producer))
	 body ...))))

  (define-syntax with-sxml-path
    (syntax-rules ()
      ((_ xpath body ...)
       (with-sxml (select 'xpath) body ...))))

  (define (replace-namespaces get-namespace-uri xxexpr)
    (define (replace-prefix prefix)
      (get-namespace-uri (and prefix (string->symbol prefix))))
    (define (replace-content node)
      (map replace-child node))
    (define (replace-attr attr)
      (let ((prefix+localname (split-qname (car attr))))
	(cons (join-qname (replace-prefix (car prefix+localname)) (cdr prefix+localname))
	      (cdr attr))))
    (define (replace-child child)
      (xxexpr-child-case child
			 (lambda (prefix localname attrs body)
			   (make-xxexpr-edge (replace-prefix prefix)
					     localname
					     (map replace-attr attrs)
					     (replace-content body)))
			 (lambda (atom) atom)))
    (replace-content xxexpr))

  ;; We often need to expand namespace prefixes into the actual namespace,
  ;; to make it easier to convert to a (Java) DOM, for example.
  (define (expand-prefixes xxexpr)
    (replace-namespaces (lambda (p)
			  (cond
			   ((assq p (current-namespaces-for-xpath)) => cdr)
			   (else (error+ "Unknown SXML prefix"
					 p
					 (current-namespaces-for-xpath)))))
			xxexpr))

  ;; --------------------------------------------------

  (define (split-qname qname)
    (let* ((ge-str (symbol->string qname))
	   (last-colon (string-index-right ge-str #\:)))
      (if last-colon		       ; GE is indeed an extended name
	  (cons (substring ge-str 0 last-colon)
		(substring ge-str (+ last-colon 1) (string-length ge-str)))
	  (cons #f ge-str))))

  (define (join-qname prefix-or-false localname)
    (string->symbol (if prefix-or-false
			(string-append prefix-or-false ":" localname)
			localname)))

  (define (xxexpr-child-case x k-edge k-atom)
    (if (pair? x)
	(let* ((tag (car x))
	       (split-tag (split-qname tag))
	       (attrs* (and (pair? (cdr x))
			    (let ((a (cadr x)))
			      (cond
			       ((and (pair? a) (pair? (car a))) a)
			       ((and (pair? a) (eq? (car a) '@)) (cdr a))
			       ((null? a) a)
			       (else #f)))))
	       (body (if attrs*
			 (cddr x)
			 (cdr x)))
	       (attrs (or attrs* '())))
	  (k-edge (car split-tag) (cdr split-tag) attrs body))
	(k-atom x)))

  (define xxexpr-edge-use-sxml-attributes (make-parameter #f))

  (define (make-xxexpr-edge prefix-or-false localname attrs-or-false body)
    (cons (join-qname prefix-or-false localname)
	  (if (and attrs-or-false (pair? attrs-or-false))
	      (cons (if (xxexpr-edge-use-sxml-attributes)
			(cons '@ attrs-or-false)
			attrs-or-false)
		    body)
	      body)))

  ;; (visit-xxexpr (list-of visitor) xxexpr) -> xxexpr
  ;; visitor: (list predicate (value -> value))
  (define (visit-xxexpr transformer-spec xxexpr)
    (define (transform-child child)
      (let find-transformer ((spec transformer-spec))
	(if (null? spec)
	    (xxexpr-child-case child
			       (lambda (prefix localname attrs body)
				 (make-xxexpr-edge prefix localname attrs (transform-node body)))
			       (lambda (atom) atom))
	    (let ((predicate (car (car spec)))
		  (transformer (cadr (car spec))))
	      (if (predicate child)
		  (transformer child)
		  (find-transformer (cdr spec)))))))
    (define (transform-node node)
      (filter-map transform-child node))
    (transform-node xxexpr))

  ;; Return a visitor that renames a given tag to another
  (define (ge-rename-visitor from to)
    (list (tag-equals-pred from)
	  (lambda (edge) (cons to (cdr edge)))))

  ;; Return a visitor that sets the content of the element with the given tag
  ;; by applying the given procedure to the current value.  If there is not current
  ;; value (the element is empty), the empty string is given to the procedure.
  ;; It does not create the element if there is not one present.
  (define (replace-content-visitor tag value-proc)
    (list (tag-equals-pred tag)
	  (lambda (edge) (list tag (value-proc (let ((content (cdr edge)))
						 (if (null? content) "" (car content))))))))

  (define (replace-content-with-value-visitor tag value)
    (replace-content-visitor tag (lambda (x) value)))

  ;; symbol -> (value -> bool)
  (define (tag-equals-pred from)
    (lambda (edge) (and (pair? edge) (eq? (car edge) from))))

  ;; symbol -> (value -> bool)
  (define (localname-equals-pred localname)
    (lambda (possibly-edge)
      (and (pair? possibly-edge)
	   (eq? (string->symbol (cdr (split-qname (car possibly-edge)))) localname))))

  (define (or-pred . preds)
    (lambda (edge)
      (let loop ((preds preds))
	(cond
	 ((null? preds) #f)
	 (((car preds) edge))
	 (else (loop (cdr preds)))))))

  (define (ge-rename-visitor* renaming-map)
    (list (lambda (edge) (and (pair? edge)
			      (assq (car edge) renaming-map)))
	  (lambda (edge) (cons (cdr (assq (car edge) renaming-map)) (cdr edge)))))

  (define (prepend-child-to-edge edge child)
    (xxexpr-child-case edge
		       (lambda (nsuri localname attrs body)
			 (make-xxexpr-edge nsuri localname attrs (cons child body)))
		       (lambda (atom)
			 (error+ "Expected tag edge in prepend-child-to-edge"
				 edge
				 child))))

  (define (edge-body edge)
    (xxexpr-child-case edge
		       (lambda (nsuri localname attrs body) body)
		       (lambda (atom) '())))
  )
