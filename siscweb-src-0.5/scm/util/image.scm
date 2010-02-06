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
;;; Portions created by the Initial Developer are Copyright (C) 2005
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


(module util/image
  (image/get-dimensions image/scale image/fit
   <interpolation-bilinear> <interpolation-nearest-neighbor>)

  (import s2j)

  (define-java-classes
    (<affine-transform> |java.awt.geom.AffineTransform|)
    (<affine-transform-op> |java.awt.image.AffineTransformOp|)
    (<buffered-image> |java.awt.image.BufferedImage|))

  (define-generic-java-methods
    filter
    get-height
    get-width
    get-scale-instance)


  (define (image/get-dimensions image)
    (cons (->number (get-width image))
          (->number (get-height image))))


  (define (static class name)
    ((generic-java-field-accessor name) (java-null class)))

  (define <interpolation-nearest-neighbor> (static <affine-transform-op> '|TYPE_NEAREST_NEIGHBOR|))
  (define <interpolation-bilinear> (static <affine-transform-op> '|TYPE_BILINEAR|))


  (define (image/scale image f type)
    (let* ((jf (->jdouble f))
           (r (java-new
               <affine-transform-op>
               (get-scale-instance (java-null <affine-transform>) jf jf)
               type)))
      (filter r image (java-null <buffered-image>))))

  (define (image/fit image dim type)
    (let* ((id (image/get-dimensions image))
           (iw (car id))
           (ih (cdr id))
           (ir (/ iw ih))
           (tw (car dim))
           (th (cdr dim))
           (tr (/ tw th))
           (f (if (> ir tr)
                  (/ tw iw)
                  (/ th ih))))
      (image/scale image f type)))
  )
