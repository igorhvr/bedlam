#| The authors of this work have released all rights to it and placed it
in the public domain under the Creative Commons CC0 1.0 waiver
(http://creativecommons.org/publicdomain/zero/1.0/).

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

Retrieved from: http://en.literateprograms.org/Merge_sort_(Scheme)?oldid=16653
|#

(define (split ls)
  (letrec ([split-h (lambda (ls ls1 ls2)
                      (cond
                        [(or (null? ls) (null? (cdr ls)))
                         (cons (reverse ls2) ls1)]
                        [else (split-h (cddr ls)
                                (cdr ls1) (cons (car ls1) ls2))]))])
    (split-h ls ls '())))

(define (merge pred ls1 ls2)
  (cond
    [(null? ls1) ls2]
    [(null? ls2) ls1]
    [(pred (car ls1) (car ls2))
     (cons (car ls1) (merge pred (cdr ls1) ls2))]
    [else (cons (car ls2) (merge pred ls1 (cdr ls2)))]))

(define (merge-sort pred ls)
  (cond
    [(null? ls) ls]
    [(null? (cdr ls)) ls]
    [else (let ([splits (split ls)])
            (merge pred
              (merge-sort pred (car splits))
              (merge-sort pred (cdr splits))))]))
