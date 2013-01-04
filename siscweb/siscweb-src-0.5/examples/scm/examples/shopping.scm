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

(require-library 'siscweb/bindings)
(require-library 'siscweb/xhtml)

(module examples/shopping
  (shopping)

  (import hashtable)

  (import siscweb/bindings)
  (import siscweb/xhtml)


  ;;; *** MODEL ***

  ;; cart object
  (define (make-cart)
    (make-hashtable equal? #t))

  (define (add-to-cart! cart item)
    (let ((qty (hashtable/get cart item 0)))
      (hashtable/put! cart item (+ 1 qty))))

  (define (remove-cart-item! cart item)
    (hashtable/remove! cart item))

  (define (update-cart-item-qty! cart item qty)
    (if (<= 0 qty)
        (remove-cart-item! cart item)
        (hashtable/put! cart item qty)))

  (define (cart->hashtable cart)
    cart)


  ;; items for sale - the names suffice for now; inventory and prices
  ;; could be easily added;
  (define item-names
    (alist->hashtable
     `((cat . "Cat")
       (dog . "Dog")
       (hamster . "Hamster"))))

  (define (get-item-name item)
    (hashtable/get item-names item "unknown"))

  (define (item-names->hashtable)
    item-names)



  ;;; *** CONTROLLER ***

  ;; entry point
  (define (shopping req)
    (set! req #f)
    ;; instantiates a new cart and uses it to shop; instead it could
    ;; also lookup a cart from persistent storage
    (let ((cart (make-cart)))
      (shop! cart)
      (checkout! cart)))


  ;; shops using a cart; the cart is mutated because otherwise it
  ;; would snap back in time when using the back button in the browser
  (define (shop! cart)
    (let-bindings ((add? "add?")
                   (item "item")
                   (done? "done?"))
        (get-bindings
         (send-xhtml/suspend
          (make-choose-page))) ; the page for the user

      (cond
       ;; adding an item?
       (add?
        (add-to-cart! cart item)
        (manage-cart! cart)
        (shop! cart))
       ;; if done, returns the cart
       (done? cart)
       ;; loops on unknown actions
       (else
        (shop! cart)))))

  ;; manages a cart, e.g. item removals; quantity updates could be
  ;; easily implemented;
  (define (manage-cart! cart)
    (let-bindings ((done? "done?")
                   (remove? "remove?")
                   (item "item"))
        (get-bindings
          (send-xhtml/suspend
           (make-manage-cart-page cart))) ; the page for the user
      (cond
       ;; are we done?
       (done? #t)
       ;; user asked to remove an item?
       (remove?
        (remove-cart-item! cart item)
        (manage-cart! cart))
       ;; loop on unknown commands
       (else (manage-cart! cart)))))


  ;; checkout operation; right now it just displays the cart
  (define (checkout! cart)
    (send-xhtml/back
     (make-checkout-page cart)))



  ;;; *** VIEW ***

  ;; creates a suspendable page for the user
  ;; to choose items to place in the cart
  (define (make-choose-page)
    (lambda (k-url)
      `(*TOP*
        (*PI* xml "version=\"1.0\"")
        (*DTD-INFO/PUBLIC* "html"
                           "-//W3C//DTD XHTML 1.0 Strict//EN"
                           "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd")
        (html (@ (xmlns "http://www.w3.org/1999/xhtml")
                 (xml:lang "en") (lang "en"))
         (head
          (title "Shopping")
          (link (@ (href-c "/css/default.css")
                   (rel "stylesheet")
                   (type "text/css"))))
         (body
          (h3 "Shopping")
          (p "Choose:")
          (ul
           ,@(hashtable/map
              (lambda (item item-name)
                `(li ,item-name
                  " - "
                  (a (@ (bindings ((add? . "add") (item . ,item))))
                   "add to cart")))
              (item-names->hashtable)))
          (a (@ (bindings ((done? . "done"))))
           "checkout >")
          (p (a (@ (href-c "/")) "^ Home")))))))


  ;; creates a suspendable page for the user to manage the cart
  (define (make-manage-cart-page cart)
    (lambda (k-url)
      `(*TOP*
        (*PI* xml "version=\"1.0\"")
        (*DTD-INFO/PUBLIC* "html"
                           "-//W3C//DTD XHTML 1.0 Strict//EN"
                           "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd")
        (html (@ (xmlns "http://www.w3.org/1999/xhtml")
                 (xml:lang "en") (lang "en"))
         (head
          (title "Manage Cart")
          (link (@ (href-c "/css/default.css")
                   (rel "stylesheet")
                   (type "text/css"))))
         (body
          (h3 "Manage Cart")
          (p "Your cart contains:")
          (ul
           ,@(hashtable/map
              (lambda (item qty)
                `(li
                  ,qty
                  " of "
                  ,(get-item-name item)
                  " - "
                  (a (@ (bindings ((remove? . "remove") (item . ,item))))
                   "remove from cart")))
              (cart->hashtable cart)))
          (p
           (a (@ (bindings ((done? . "done"))))
            "< continue shopping"))
          (p (a (@ (href-c "/")) "^ Home")))))))



  ;; creates a non-suspendable page with the cart contents
  (define (make-checkout-page cart)
    `(*TOP*
      (*PI* xml "version=\"1.0\"")
      (*DTD-INFO/PUBLIC* "html"
                         "-//W3C//DTD XHTML 1.0 Strict//EN"
                         "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd")
      (html (@ (xmlns "http://www.w3.org/1999/xhtml")
               (xml:lang "en") (lang "en"))
       (head
        (title "Checkout")
        (link (@ (href-c "/css/default.css")
                 (rel "stylesheet")
                 (type "text/css"))))
       (body
        (h3 "Checkout")
        (p "Your cart contains:")
        (ul
         ,@(hashtable/map
            (lambda (item qty)
              `(li ,qty " of " ,(get-item-name item)))
            (cart->hashtable cart)))
        (p (a (@ (href-c "/")) "^ Home"))))))

  )
