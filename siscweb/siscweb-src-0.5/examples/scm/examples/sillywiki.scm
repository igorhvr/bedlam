;;; The contents of this file are subject to the Mozilla Public License Version
;;; 1.1 (the "License"); you may not use this file except in compliance with
;;; the License. You may obtain a copy of the License at
;;; http://www.mozilla.org/MPL/
;;;
;;; Software distributed under the License is distributed on an "AS IS" basis,
;;; WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
;;; for the specific language governing rights and limitations under the
;;; License.
;;;
;;; The Original Code is SISCweb.
;;;
;;; The Initial Developer of the Original Code is Alessandro Colomba.
;;; Portions created by the Initial Developer are Copyright (C) 2005-2006
;;; Alessandro Colomba. All Rights Reserved.
;;;
;;; Contributor(s):
;;;
;;; Alternatively, the contents of this file may be used under the terms of
;;; either the GNU General Public License Version 2 or later (the "GPL"), or
;;; the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
;;; in which case the provisions of the GPL or the LGPL are applicable instead
;;; of those above. If you wish to allow use of your version of this file only
;;; under the terms of either the GPL or the LGPL, and not to allow others to
;;; use your version of this file under the terms of the MPL, indicate your
;;; decision by deleting the provisions above and replace them with the notice
;;; and other provisions required by the GPL or the LGPL. If you do not delete
;;; the provisions above, a recipient may use your version of this file under
;;; the terms of any one of the MPL, the GPL or the LGPL.

;;; This is a very simplistic wiki to show the use some of the most interesting
;;; features of the siscweb framework, such as "let-bindings" and "url-bindings"
;;; Truly a wiki is not the best application to write in continuation-based style,
;;; yet it can be done for no additional complexity cost.
;;; In the end the only drawback is that every hyperlink is opaque rather than
;;; in the form "wiki?page=PageName"; while in a wiki it hampers bookmarking,
;;; in a web application it is an extra layer of security.

(require-library 'sisc/libs/srfi/srfi-1)

(require-library 'siscweb/xhtml)
(require-library 'siscweb/bindings)
(require-library 'util/regexp)

(module examples/sillywiki
  (sillywiki)

  (import hashtable)

  (import srfi-1)

  (import siscweb/xhtml)
  (import siscweb/bindings)
  (import util/regexp)


  ;; the page repository.. a couple of wiki pages to go by
  (define pages (alist->hashtable
                 '(("MainPage" . "Would you rather I say HelloWorld or SalveMundo?")
                   ("HelloWorld" . "Hello, world!"))
                 equal? #t))

  ;; fetches a page from the repository; returns "" if the
  ;; page does not exist
  (define (fetch-page-text page-name)
    (hashtable/get pages page-name))

  ;; stores a wiki page in the repository
  (define (store-page page-name page-text)
    (hashtable/put! pages page-name page-text))

  ;; determins whether the indicated page exists in
  ;; the repository
  (define (exists-page? page-name)
    (if (hashtable/get pages page-name) #t #f))

  ;; returns a list of all page names
  (define (get-all-page-names)
    (hashtable/keys pages))

  ;; views the given page, then reads the request and either:
  ;;   a) goes to the page the user has chosen (allowing the
  ;;      user to create it if it doesn't exist yet)
  ;;   b) edits the given page if the user has so asked.
  ;; precondition: the given page-name must exists
  (define (view-page page-name)
    ;; page-name -> does not return
    (let-bindings ((next-page-name "page")
                   (edit? "edit"))
                  (get-bindings
                     (send-xhtml/suspend (make-view-page page-name)))
      (cond (next-page-name ;; user clicked on a page link?
             (cond ((exists-page? next-page-name)
                    (view-page next-page-name))
                   (else
                    (let ((new-page-text (read-new-page next-page-name)))
                      (cond (new-page-text
                             (store-page next-page-name new-page-text)
                             (view-page next-page-name))
                            (else
                             (view-page page-name)))))))
            (edit? ;; user wants to edit this page?
             (edit-page! page-name)
             (view-page page-name))
            (else ;; otherwise just redisplay the current page
             (view-page page-name)))))

  ;; displays all pages
  (define (view-page-list)
    (send-xhtml/suspend
     (lambda (k-url)
       `(*TOP*
         (*PI* xml "version=\"1.0\"")
         (*DTD-INFO/PUBLIC* "html"
                            "-//W3C//DTD XHTML 1.0 Strict//EN"
                            "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd")
         (html (@ (xmlns "http://www.w3.org/1999/xhtml")
                  (xml:lang "en") (lang "en"))
          (head
           (title "All Pages")
           (link (@ (href-c "/css/default.css")
                    (rel "stylesheet")
                    (type "text/css"))))
          (body
           (h2 "All Pages")
           (ul
            ,@(map (lambda (page-name)
                     `(li (a (@ (href-p ,(lambda (req)
                                           (view-page page-name))))
                             ,page-name)))
                   (get-all-page-names)))))))))


  ;; makes a page for viewing
  (define (make-view-page page-name)
    (lambda (k-url)
      (let ((page-text (fetch-page-text page-name)))
        `(*TOP*
          (*PI* xml "version=\"1.0\"")
          (*DTD-INFO/PUBLIC* "html"
                             "-//W3C//DTD XHTML 1.0 Strict//EN"
                             "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd")
          (html (@ (xmlns "http://www.w3.org/1999/xhtml")
                   (xml:lang "en") (lang "en"))
           (head
            (title ,page-name)
            (link (@ (href-c "/css/default.css")
                     (rel "stylesheet")
                     (type "text/css"))))
           (body
            (h2 ,page-name)
            (p ,(htmlize page-text))
            (p (a (@ (bindings ((edit . ,page-name))))
                  "Edit"))
            (p (a (@ (href-p ,(lambda (req) (view-page-list))))
                  "View All Pages"))
            (p (p (a (@ (href-c "/")) "^ Home")))))))))


  ;; given the text of a wiki page, it creates an html fragment
  ;; with hyperlinks for each wiki word
  (define (htmlize page-text)
    `(pre
      ,@(text->sxml page-text)))


  ;; returns an sxml fragment of the given text translated to html
  ;; according to wiki rules; right now it just converts wiki words
  ;; to bindings links
  (define (text->sxml text)
    (let ((matches (regexp-match-positions "(([A-Z][a-z0-9]+){2,})" text)))
      (if (not matches)
          `(,text)
          (reverse
           (pair-fold
            (lambda (lst knil)
              (let* ((x (car lst))
                     (rest (cdr lst))
                     (wikiword (substring text (car x) (cdr x))))
                `(,(if (null? rest)
                       (substring text (cdr x) (string-length text))
                       (substring text (cdr x) (car (car rest))))
                  (a (@ (bindings ((page . ,wikiword)))) ,wikiword)
                  . ,knil)))
            (list (substring text 0 (car (car matches))))
            matches)))))



  ;; reads the content of a wiki page from the user, or returns #f
  ;; if the user cancelled the operation
  (define (read-new-page page-name)
    ;; page-name -> page-text (read from user)
    (let-bindings ((save? "save")
                   (page-text "page-text"))
                  (get-bindings
                   (send-xhtml/suspend
                    (make-edit-page page-name "")))
      (if save?
          page-text
          #f)))


  (define (edit-page! page-name)
    (let-bindings ((save? "save")
                   (page-text "page-text"))
                  (get-bindings
                   (send-xhtml/suspend
                    (make-edit-page page-name (fetch-page-text page-name))))
      (when save?
        (store-page page-name page-text))))


  ;; returns an edit page
  (define (make-edit-page page-name page-text)
    (lambda (k-url)
      `(*TOP*
        (*PI* xml "version=\"1.0\"")
        (*DTD-INFO/PUBLIC* "html"
                           "-//W3C//DTD XHTML 1.0 Strict//EN"
                           "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd")
        (html (@ (xmlns "http://www.w3.org/1999/xhtml")
                 (xml:lang "en") (lang "en"))
         (head
          (title ,page-name)
          (link (@ (context-href "/css/default.css")
                   (rel "stylesheet")
                   (type "text/css"))))
         (body
          (h2 ,page-name)
          (form (@ (action-e ,k-url) (method "post"))
                (textarea (@ (name "page-text") (rows "15") (cols "65"))
                 ,(fetch-page-text page-name))
                (br)
                (input (@ (type "submit") (name "save") (value "Store")))
                (input (@ (type "submit") (name "cancel") (value "Cancel")))))))))

  (define (sillywiki request)
    (view-page "MainPage"))
  )
