(module schelog
    (_
     (%let schelog:make-ref)
     (%or schelog:deref*)
     (%and schelog:deref*)
     (%cut-delimiter schelog:deref*)
     (%rel schelog:deref*)
     (%is schelog:deref* schelog:ref? schelog:unbound-ref?)
     (%assert schelog:deref*)
     (%assert-a schelog:deref*)
     (%free-vars)
     (%which schelog:deref* schelog:*more-k* schelog:*more-fk*)
     (%more schelog:deref* schelog:*more-k* schelog:*more-fk*)
     %true
     %fail
     %not
     %var
     %nonvar
     %=
     %/=
     %==
     %/==
     %=:=
     %>
     %>=
     %<
     %<=
     %=/=
     %constant
     %compound
     %freeze
     %melt
     %melt-new
     %empty-rel
     %bag-of
     %set-of
     %bag-of-1
     %set-of-1
     %member
     %if-then-else
     %append
     %repeat)
  (include "schelog.scm"))
