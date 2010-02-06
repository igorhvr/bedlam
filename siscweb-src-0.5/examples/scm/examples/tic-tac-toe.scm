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

;;; This tic-tac-toe program uses schelog to play against the human It
;;; substitutes the previous two-player example by the same name
;;; written by Scott G. Miller, which remains available at:
;;; http://cvs.sourceforge.net/viewcvs.py/siscweb/siscweb/examples/scm/examples/tic-tac-toe.scm?rev=1.2.4.1&view=markup

(require-library 'sisc/libs/srfi/srfi-1) ; list library
(require-library 'sisc/libs/srfi/srfi-42) ; eager comprehensions

(require-library 'siscweb/xhtml)
(require-library 'schelog)

(module examples/tic-tac-toe
  (tic-tac-toe)

  (import srfi-1)
  (import srfi-42)

  (import siscweb/bindings)
  (import siscweb/xhtml)

  (import schelog)


  (define (tic-tac-toe req)
    (set! req #f)
    (%which (winner)
      (%play (make-board) 'x winner)))


  ;; controller (in prolog? sure, why not)
  (define %play
    (%rel (board board2 player winner dummy)
      ;; in case of winner or draw,
      ;; shows the final board
      ((board player 'x)
       (%xwins board)
       (%is dummy (gameover board 'x)))
      ((board player 'o)
       (%owins board)
       (%is dummy (gameover board 'o)))
      ((board player 'd)
       (%gameover board)
       (%is dummy (gameover board 'd)))

      ;; Otherwise plays human or computer
      ((board 'o winner)
       (%move-human board 'o board2)
       (%play board2 'x winner))
      ((board 'x winner)
       (%move-computer board 'x board2)
       (%play board2 'o winner))))


  ;; asks the human for a move
  (define %move-human
    (%rel (board sign board2 pos)
      ((board sign board2)
       (%is pos (read-move board sign))
       (%is board2 (board-move board pos sign)))))

  ;; makes a move based on the strategy (see below)
  (define %move-computer
    (%rel (board sign board2 pos)
      ((board sign board2)
       ;; NB: sign is not used b/c the logic
       ;; is hard-coded for the computer to play X
       (%make-move board pos)
       (%is board2 (board-move board pos sign)))))



  ;; view
  (define (read-move board sign)
    (extract-single-binding
     'pos
     (get-bindings
      (send-xhtml/suspend
       (lambda (k-url)
         (make-read-move-page board sign))))))

  (define (make-read-move-page board sign)
    `(*TOP*
      (*PI* xml "version=\"1.0\"")
      (*DTD-INFO/PUBLIC* "html"
                         "-//W3C//DTD XHTML 1.0 Strict//EN"
                         "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd")
      (html (@ (xmlns "http://www.w3.org/1999/xhtml")
               (xml:lang "en") (lang "en"))
       (head
        (title "Tic Tac Toe")
        (link (@ (href-c "/css/default.css")
                 (rel "stylesheet")
                 (type "text/css"))))
       (body
        (h3 "Tic Tac Toe")
        ,(make-board-table board sign)
        (p (a (@ (href-p ,tic-tac-toe)) "Start a new game >"))
        (p (a (@ (href-c "/")) "^ Home"))))))


  (define (gameover board winner)
    (send-xhtml/back
     `(*TOP*
       (*PI* xml "version=\"1.0\"")
       (*DTD-INFO/PUBLIC* "html"
                          "-//W3C//DTD XHTML 1.0 Strict//EN"
                          "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd")
       (html (@ (xmlns "http://www.w3.org/1999/xhtml")
                (xml:lang "en") (lang "en"))
        (head
         (title "Tic Tac Toe")
         (link (@ (href-c "/css/default.css")
                  (rel "stylesheet")
                  (type "text/css"))))
        (body
         (h3 "Tic Tac Toe")
         (h2 ,(case winner
                ((x) "X wins")
                ((o) "O wins")
                ((d) "DRAW")))
         ,(make-board-table board 'n)
         (p (a (@ (href-p ,tic-tac-toe)) "Start a new game >"))
         (p (a (@ (href-c "/")) "^ Home")))))))


  (define (make-board-table board sign)
    `(table (@ (style "font-size: xx-large; border-style: 1px solid grey;")
               (border 1) (cellspacing 0))
      ,@(list-ec
         (:range i 0 3)
         `(tr
           ,@(list-ec
              (:range j 0 3)
              `(td (@ (class "tic-tac-toe"))
                ,(let ((pos (+ (* 3 i) j 1))) ; 1 to 9
                   (case (board-ref board pos)
                     ((x) "X")
                     ((o) "O")
                     (else
                      (case sign
                        ((x o)
                          `(a (@ (class "tic-tac-toe")
                                 (bindings ((pos . ,pos))))
                              ,(if (eq? sign 'x) "X" "O")))
                        (else
                         `(& nbsp))))))))))))


  ;;; model

  ;; a literal is fine as long as we don't alter it
  (define (make-board)
    '(e e e e e e e e e))

  ;; returns the cell value in position 1 <= n <= 9
  (define (board-ref board pos)
    (list-ref board (- pos 1)))

  ;; returns a zipper list reflecting the move
  (define (board-move board pos sign)
    (append (take board (- pos 1))
            (cons sign
                  (drop board pos))))



  ;; play strategy; ported to schelog from
  ;; www.cs.fsu.edu/~engelen/courses/COP402002/notes.pdf
  ;; the map:
  ;; 1 2 3
  ;; 4 5 6
  ;; 7 8 9

  ;; a cell; it doubles as a last-resort cell choice
  (define %cell
    (%rel (n) ((5)) ((1)) ((3)) ((7)) ((9)) ((2)) ((4)) ((6)) ((8))))

  ;; the winning lines on the board, ordered
  (define %ordered-line
    (%rel ()
      ((1 2 3))
      ((4 5 6))
      ((7 8 9))
      ((1 4 7))
      ((2 5 8))
      ((3 6 9))
      ((1 5 9))
      ((3 5 7))))

  ;; a winning line is a permutation of any winning ordered line
  (define %line
    (%rel (a b c)
      ((a b c)
       (%or (%ordered-line a b c)
            (%ordered-line a c b)
            (%ordered-line b a c)
            (%ordered-line b c a)
            (%ordered-line c a b)
            (%ordered-line c b a)))))

  ;; is a given position taken by player X?
  (define %x
    (%rel (board pos)
      ((board pos)
       (%cell pos)
       (%is #t (eq? 'x (board-ref board pos))))))

  ;; is a given position taken by player O?
  (define %o
    (%rel (board pos)
      ((board pos)
       (%cell pos)
       (%is #t (eq? 'o (board-ref board pos))))))

  ;; is a given position occupied?
  (define %full
    (%rel (board pos)
      ((board pos)
       (%or (%x board pos)
            (%o board pos)))))

  ;; is a given position empty?
  (define %empty
    (%rel (board pos)
      ((board pos)
       (%cell pos)
       (%not (%full board pos)))))

  ;; a winning move is one that completes a winning line
  (define %win
    (%rel (board a b c)
      ((board a)
       (%line a b c)
       (%x board b)
       (%x board c))))

  ;; a non-losing move is one that block the opponent from winning
  (define %block-win
    (%rel (board a b c)
      ((board a)
       (%line a b c)
       (%o board b)
       (%o board c))))

  ;; a move that makes a split is one at the intersection of
  ;; two winning lines, one element of each line is already
  ;; ours, and the other is still empty
  (define %split
    (%rel (board a b c d e)
      ((board a)
       (%x board b)
       (%x board c)
       (%=/= b c)
       (%line a b d)
       (%line a c e)
       (%empty board d)
       (%empty board e))))

  ;; same rule as above, but to prevent the opponent from
  ;; making a split
  (define %block-split
    (%rel (board a b c d e)
      ((board a)
       (%o board b)
       (%o board c)
       (%=/= b c)
       (%line board a b d)
       (%line board a c e)
       (%empty board d)
       (%empty board e))))

  ;; a move that builds a winning line is one that
  ;; starts from a cell already ours, and ends in a cell
  ;; that is still empty
  (define %build
    (%rel (board a b c)
      ((board a)
       (%x board b)
       (%line a b c)
       (%empty board c))))

  ;; a good move is one of the above moves, in order
  ;; of strategic importance
  (define %good
    (%rel (board pos)
      ((board pos)
       (%or (%win board pos)
            (%block-win board pos)
            (%split board pos)
            (%block-split board pos)
            (%build board pos)
            (%cell pos)))))

  ;; an actual move is a good move on an empty cell
  (define %make-move
    (%rel (board pos)
      ((board pos)
       (%good board pos)
       (%empty board pos))))

  ;; X wins if it occupies a winning line
  (define %xwins
    (%rel (board a b c)
      ((board)
       (%x board a)
       (%x board b)
       (%x board c)
       (%line a b c))))

  ;; O wins if it occupies a winning line
  (define %owins
    (%rel (board a b c)
      ((board)
       (%o board a)
       (%o board b)
       (%o board c)
       (%line a b c))))

  ;; the game is over if no cells are free
  (define %gameover
    (%rel (board any)
      ((board)
       (%not
        (%and (%cell any)
              (%empty board any))))))

  ;; a draw is a gameover with no winners
  (define %draw
    (%rel (board any)
      ((board)
       (%not (%xwins board))
       (%not (%owins board))
       (%gameover board))))
  )
