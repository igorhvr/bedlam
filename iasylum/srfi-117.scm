; Keyword objects.
(module iasylum/srfi-117
  ( 
   ; Constructor
   make-list-queue
   list-queue
   list-queue-copy
   list-queue-unfold
   list-queue-unfold-right

   ; Predicates
   list-queue?
   list-queue-empty?

   ; Acessors
   list-queue-front
   list-queue-back
   list-queue-list
   list-queue-first-last

   ; Mutators
   list-queue-add-front!
   list-queue-add-back!
   list-queue-remove-front!
   list-queue-remove-back!
   list-queue-remove-all!
   list-queue-set-list!
   list-queue-append
   list-queue-append!
   list-queue-concatenate

   ; Mapping
   list-queue-map
   list-queue-map!
   list-queue-for-each
   )
  (include "srfi-117-code.scm"))
