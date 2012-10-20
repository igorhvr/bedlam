(import debugging)

; basic infobot plugins
(define (*-is? type)
  (lambda (channel message ignore term)
    (if (eqv? #\? (string-ref term (- (string-length term) 1)))
        (set! term (substring term 
                              0 (- (string-length term) 1))))
    (and term 
         (let ([results (lookup-item dbcon type (trim term))])
           (or (not results)
               (let ([random-result (random-elem results)])
                 (format "~a~a ~a ~a" 
                              (random-elem whatis-preludes) 
                              (car random-result)
                              (cdr (assq (cadr random-result) 
                                         '((what . is) (where . "is at"))))
                              (cddr random-result))))))))


(define (ignorable? tokens definition)
  (or (not definition)
      (and (> (length tokens) (length (tokenize definition)))
           (> (length tokens) 1))
      (and (= 1 (length tokens)) (memq (car tokens) ignored-words))))

(define (learn type)
  (lambda (channel message term definition)
    (if (not term)
        'continue
        (let ([tt (tokenize term)])
          (or (and (not (eq? type 'where)) (ignorable? tt definition) 'continue)
            (if (store-item dbcon type term definition)
                (random-elem learn-responses) 
                (random-elem knewthat-responses)))))))

(define (learn-aka channel message term definition)
  (or (and (ignorable? (tokenize term) definition)
           'continue)
      (if (store-aka dbcon term definition)
          (random-elem learn-responses) 
          (random-elem knewthat-responses))))

(define (forget channel message ignore term)
  (display term) (newline)
  (remove-items dbcon term)
  (random-elem forget-responses))

(define (lookup-item conn type key)
  (let* ([stmt (jdbc/prepare-statement conn
                  (format 
                    (string-append
                      "SELECT knowledge.key, knowledge.type, knowledge.data FROM knowledge LEFT JOIN aka ON "
                      "    (aka.key ilike ? AND aka.data ilike knowledge.key)"
                      " WHERE ((aka.key IS NULL AND knowledge.key ilike ?) "
                      "    OR (aka.key IS NOT NULL)) "
                      "   ~a") 
                    (if type (format "AND type='~a'"
                                                   type) "")))]
                  
         [results
	  (begin (set-string stmt (->jint 1) (->jstring key))
                 (set-string stmt (->jint 2) (->jstring key))
                 (jdbc/execute-query stmt))])
    (and (not (null? results)) 
         (ordered-stream-map (lambda (item)
                               (cons (item '1)
                                     (cons (string->symbol (item '2)) 
                                           (item '3))))
                     results))))

(define (store-item conn type key data)
  (let ([pstmt (jdbc/prepare-statement conn
                "INSERT INTO knowledge VALUES(?,trim(?),?)")])
    (set-string pstmt (->jint 1) (->jstring type))
    (set-string pstmt (->jint 2) (->jstring key))
    (set-string pstmt (->jint 3) (->jstring data))
    (with/fc (lambda (m e) #f) (lambda () (jdbc/execute pstmt)))))

(define (store-aka conn data key)
  (let ([pstmt (jdbc/prepare-statement conn
                "INSERT INTO aka VALUES(trim(?),trim(?))")])
    (set-string pstmt (->jint 1) (->jstring key))
    (set-string pstmt (->jint 2) (->jstring data))
    (with/fc (lambda (m e) #f) (lambda () (jdbc/execute pstmt)))))

(define (remove-items conn key)
  (let ([pstmt (jdbc/prepare-statement conn
                "DELETE FROM knowledge WHERE key=?")])
    (set-string pstmt (->jint 1) (->jstring key))
    (with/fc (lambda (m e) #f) (lambda () (jdbc/execute pstmt)))))

(trace 'remove-items)
(define whatis-preludes
  '("I've heard " 
    "Someone once said "
    ""
    "I could be wrong, but "
    "Its been said that "
    "Last time I checked "
    "From what I understand, "))

(define knewthat-responses
  '("Thats what I heard."
    "Yep, I know."
    "Thats very true."))

(define learn-responses
  '("Got it."
    "Okay."
    "Understood."
    "I'll keep that in mind."
    "So noted."))

(define forget-responses
  '("Okay."
    "Consider it forgotten."))

(define didntknow-responses
  '("Already done."
    "Never knew it."))

(define ignored-words '(
about
after
all
also
an
and
another
any
are
as
at
be
$
because
been
before
being
between
both
but
by
came
can
come
could
did
do
does
each
else
for
from
get
got
has
had
he
have
her
here
him
himself
his
how
if
in
into
is
it
its
just
like
make
many
me
might
more
most
much
must
my
never
now
of
on
only
or
other
our
out
over
re
said
same
see
should
since
so
some
still
such
take
than
that
the
their
them
then
there
these
they
this
those
through
to
too
under
up
use
very
want
was
way
we
well
were
what
when
where
which
while
who
why
will
with
would
you
your))