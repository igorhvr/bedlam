(#%program
  ((_make-native-parameter . 1))
  ()
  (_make-native-parameter)
  (#%define hedged-inlining
    (_make-native-parameter "hedgedInlining")))
(#%program
  ((hedged-inlining . 1))
  ()
  (hedged-inlining)
  (hedged-inlining #f))
(#%program () () () #!void)
(#%program
  ((map-cdr . 1)
   (for-each . 1)
   (map-car . 1)
   (|%%_f7IgJHTds_proc| . 2)
   (apply . 2)
   (cons . 1)
   (|%%_f70oNNSds_lists| . 4)
   (|%%_f7mkLeTds_ls1| . 2)
   (null? . 1))
  ((|%%_f70oNNSds_lists| . 1))
  (cons map-car apply map-cdr for-each null?)
  (#%define for-each
    (#%lambda #t
      (|%%_f7IgJHTds_proc|
        |%%_f7mkLeTds_ls1|
        .
        |%%_f70oNNSds_lists|)
      ()
      (#%if (null? |%%_f7mkLeTds_ls1|)
        #!void
        (#%begin
          (#%set! |%%_f70oNNSds_lists|
            (cons |%%_f7mkLeTds_ls1| |%%_f70oNNSds_lists|))
          (apply |%%_f7IgJHTds_proc|
                 (map-car |%%_f70oNNSds_lists|))
          (apply for-each
                 |%%_f7IgJHTds_proc|
                 (map-cdr |%%_f70oNNSds_lists|)))))))
(#%program
  ((|%%_f72dH8Uds_x| . 1) (eq? . 1))
  ()
  (eq?)
  (#%define eof-object?
    (#%lambda #t
      (|%%_f72dH8Uds_x|)
      ()
      (eq? |%%_f72dH8Uds_x| #!eof))))
(#%program
  ((|%%_f7o9FBUds_x| . 1))
  ()
  ()
  (#%define not
    (#%lambda #t
      (|%%_f7o9FBUds_x|)
      ()
      (#%if |%%_f7o9FBUds_x| #f #t))))
(#%program
  ((|%%_f7K5D2Vds_port| . 1)
   (display . 1)
   (apply . 1))
  ()
  (display apply)
  (#%define newline
    (#%lambda #t
      |%%_f7K5D2Vds_port|
      ()
      (apply display #\newline |%%_f7K5D2Vds_port|))))
(#%program
  ((|%%_f7q-yYVds_ls| . 1)
   (car . 1)
   (cons . 1)
   (cdr . 1)
   (|%%_f742BvVds_iter| . 2)
   (|%%_f76TuSWds_acc| . 2)
   (|%%_f7MWwpWds_ls| . 3)
   (null? . 1))
  ((|%%_f742BvVds_iter| . 1))
  (cdr car cons null?)
  (#%define reverse
    (#%letrec #t
      ((|%%_f742BvVds_iter|
         (#%lambda #t
           (|%%_f7MWwpWds_ls| |%%_f76TuSWds_acc|)
           (|%%_f742BvVds_iter|)
           (#%if (null? |%%_f7MWwpWds_ls|)
             |%%_f76TuSWds_acc|
             (|%%_f742BvVds_iter|
               (cdr |%%_f7MWwpWds_ls|)
               (cons (car |%%_f7MWwpWds_ls|) |%%_f76TuSWds_acc|))))))
      ()
      (#%lambda #t
        (|%%_f7q-yYVds_ls|)
        (|%%_f742BvVds_iter|)
        (|%%_f742BvVds_iter| |%%_f7q-yYVds_ls| ())))))
(#%program
  ((|%%_f7OLqMXds_s| . 1)
   (cdr . 1)
   (|%%_f7QAk7Zds_d| . 1)
   (|%%_f7sPsjXds_iter| . 2)
   (set-cdr! . 1)
   (|%%_f7uEmGYds_r| . 2)
   (|%%_f78IodYds_s| . 4)
   (null? . 1))
  ((|%%_f7sPsjXds_iter| . 1))
  (set-cdr! cdr null?)
  (#%define reverse!
    (#%letrec #t
      ((|%%_f7sPsjXds_iter|
         (#%lambda #t
           (|%%_f78IodYds_s| |%%_f7uEmGYds_r|)
           (|%%_f7sPsjXds_iter|)
           (#%if (null? |%%_f78IodYds_s|)
             |%%_f7uEmGYds_r|
             ((#%lambda #t
                (|%%_f7QAk7Zds_d|)
                (|%%_f7uEmGYds_r|
                  |%%_f78IodYds_s|
                  |%%_f7sPsjXds_iter|)
                (#%begin
                  (set-cdr! |%%_f78IodYds_s| |%%_f7uEmGYds_r|)
                  (|%%_f7sPsjXds_iter|
                    |%%_f7QAk7Zds_d|
                    |%%_f78IodYds_s|)))
              (cdr |%%_f78IodYds_s|))))))
      ()
      (#%lambda #t
        (|%%_f7OLqMXds_s|)
        (|%%_f7sPsjXds_iter|)
        (|%%_f7sPsjXds_iter| |%%_f7OLqMXds_s| ())))))
(#%program
  ((cdr . 1)
   (map-car . 1)
   (caar . 1)
   (cons . 1)
   (|%%_f7axiAZds_ls| . 3)
   (null? . 1))
  ()
  (cons caar cdr map-car null?)
  (#%define map-car
    (#%lambda #t
      (|%%_f7axiAZds_ls|)
      ()
      (#%if (null? |%%_f7axiAZds_ls|)
        ()
        (cons (caar |%%_f7axiAZds_ls|)
              (map-car (cdr |%%_f7axiAZds_ls|)))))))
(#%program
  ((cdr . 1)
   (map-cdr . 1)
   (cdar . 1)
   (cons . 1)
   (%%_f7wtg1-ds_ls . 3)
   (null? . 1))
  ()
  (cons cdar cdr map-cdr null?)
  (#%define map-cdr
    (#%lambda #t
      (%%_f7wtg1-ds_ls)
      ()
      (#%if (null? %%_f7wtg1-ds_ls)
        ()
        (cons (cdar %%_f7wtg1-ds_ls)
              (map-cdr (cdr %%_f7wtg1-ds_ls)))))))
(#%program
  ((|%%_f7Ue8R_ds_list1| . 2)
   (%%_f7eb6i0es_proc . 2)
   (%%_f7yiao_ds_lists . 2)
   (map-car . 1)
   (apply . 1)
   (|%%_f7iRV_2es_lists| . 2)
   (map-cdr . 1)
   (|%%_f7CYZ52es_proc| . 2)
   (|%%_f7cmcX-ds_loop| . 2)
   (|%%_f7ENTs3es_c| . 2)
   (|%%_f7YUXy2es_list1| . 3)
   (car . 2)
   (cons . 2)
   (cdr . 2)
   (|%%_f7A74L0es_proc| . 2)
   (|%%_f7Speu-ds_map1| . 2)
   (|%%_f7g00F1es_acc| . 2)
   (reverse . 2)
   (|%%_f7W32c1es_list| . 3)
   (null? . 3))
  ((|%%_f7cmcX-ds_loop| . 1)
   (|%%_f7Speu-ds_map1| . 1))
  (map-cdr
    apply
    map-car
    cdr
    car
    cons
    reverse
    null?)
  (#%define map
    (#%letrec #t
      ((|%%_f7Speu-ds_map1|
         (#%lambda #t
           (|%%_f7A74L0es_proc|
             |%%_f7W32c1es_list|
             |%%_f7g00F1es_acc|)
           (|%%_f7Speu-ds_map1|)
           (#%if (null? |%%_f7W32c1es_list|)
             (reverse |%%_f7g00F1es_acc|)
             (|%%_f7Speu-ds_map1|
               |%%_f7A74L0es_proc|
               (cdr |%%_f7W32c1es_list|)
               (cons (|%%_f7A74L0es_proc| (car |%%_f7W32c1es_list|))
                     |%%_f7g00F1es_acc|)))))
       (|%%_f7cmcX-ds_loop|
         (#%lambda #t
           (|%%_f7CYZ52es_proc|
             |%%_f7YUXy2es_list1|
             |%%_f7iRV_2es_lists|
             |%%_f7ENTs3es_c|)
           (|%%_f7cmcX-ds_loop|)
           (#%if (null? |%%_f7YUXy2es_list1|)
             (reverse |%%_f7ENTs3es_c|)
             (|%%_f7cmcX-ds_loop|
               |%%_f7CYZ52es_proc|
               (cdr |%%_f7YUXy2es_list1|)
               (map-cdr |%%_f7iRV_2es_lists|)
               (cons (apply |%%_f7CYZ52es_proc|
                            (car |%%_f7YUXy2es_list1|)
                            (map-car |%%_f7iRV_2es_lists|))
                     |%%_f7ENTs3es_c|))))))
      ()
      (#%lambda #t
        (%%_f7eb6i0es_proc
          |%%_f7Ue8R_ds_list1|
          .
          %%_f7yiao_ds_lists)
        (|%%_f7cmcX-ds_loop| |%%_f7Speu-ds_map1|)
        (#%if (null? %%_f7yiao_ds_lists)
          (|%%_f7Speu-ds_map1|
            %%_f7eb6i0es_proc
            |%%_f7Ue8R_ds_list1|
            ())
          (|%%_f7cmcX-ds_loop|
            %%_f7eb6i0es_proc
            |%%_f7Ue8R_ds_list1|
            %%_f7yiao_ds_lists
            ()))))))
(#%program
  ((|%%_f7GCNP4es_x| . 1)
   (|%%_f7kGPm4es_g| . 1)
   (|%%_f7-JRV3es_f| . 1))
  ()
  ()
  (#%define compose2
    (#%lambda #t
      (|%%_f7-JRV3es_f| |%%_f7kGPm4es_g|)
      ()
      (#%lambda #t
        (|%%_f7GCNP4es_x|)
        (|%%_f7kGPm4es_g| |%%_f7-JRV3es_f|)
        (|%%_f7-JRV3es_f|
          (|%%_f7kGPm4es_g| |%%_f7GCNP4es_x|))))))
(#%program
  ((void . 1))
  ()
  (void)
  (#%define assq (void)))
(#%program
  ((void . 1))
  ()
  (void)
  (#%define assv (void)))
(#%program
  ((void . 1))
  ()
  (void)
  (#%define assoc (void)))
(#%program
  ((void . 1))
  ()
  (void)
  (#%define memq (void)))
(#%program
  ((void . 1))
  ()
  (void)
  (#%define memv (void)))
(#%program
  ((void . 1))
  ()
  (void)
  (#%define member (void)))
(#%program
  ((|%%_f7uPk9bes_list| . 1)
   (|%%_f78TmIaes_obj| . 1)
   (member . 1)
   (|%%_f7OWofaes_list| . 1)
   (|%%_f7s-qO9es_obj| . 1)
   (memv . 1)
   (%%_f762tl9es_list . 1)
   (|%%_f7M5vU8es_obj| . 1)
   (memq . 1)
   (%%_f7q9xr8es_alist . 1)
   (%%_f74dz-7es_obj . 1)
   (equal? . 2)
   (assoc . 1)
   (|%%_f7KgBx7es_alist| . 1)
   (|%%_f7okD47es_obj| . 1)
   (eqv? . 2)
   (assv . 1)
   (|%%_f72oFD6es_alist| . 1)
   (|%%_f7IrHa6es_obj| . 1)
   (eq? . 2)
   (assq . 1)
   (|%%_f7mvJJ5es_memn| . 4)
   (%%_f7cxaqdes_obj . 2)
   (|%%_f7SAcZces_n| . 2)
   (|%%_f7yt8Tdes_list| . 4)
   (cdr . 2)
   (|%%_f70zLg5es_assn| . 4)
   (car . 2)
   (|%%_f7aIg3ces_obj| . 2)
   (caar . 1)
   (|%%_f7QLiCbes_n| . 2)
   (|%%_f7wEewces_alist| . 4)
   (null? . 2))
  ((member . 1)
   (memv . 1)
   (memq . 1)
   (assoc . 1)
   (assv . 1)
   (assq . 1)
   (|%%_f7mvJJ5es_memn| . 1)
   (|%%_f70zLg5es_assn| . 1))
  (assq eq?
        assv
        eqv?
        assoc
        equal?
        memq
        memv
        member
        caar
        car
        cdr
        null?)
  (#%letrec #t
    ((|%%_f70zLg5es_assn|
       (#%lambda #t
         (|%%_f7QLiCbes_n|
           |%%_f7aIg3ces_obj|
           |%%_f7wEewces_alist|)
         (|%%_f70zLg5es_assn|)
         (#%if (null? |%%_f7wEewces_alist|)
           #f
           (#%if (|%%_f7QLiCbes_n|
                   (caar |%%_f7wEewces_alist|)
                   |%%_f7aIg3ces_obj|)
             (car |%%_f7wEewces_alist|)
             (|%%_f70zLg5es_assn|
               |%%_f7QLiCbes_n|
               |%%_f7aIg3ces_obj|
               (cdr |%%_f7wEewces_alist|))))))
     (|%%_f7mvJJ5es_memn|
       (#%lambda #t
         (|%%_f7SAcZces_n|
           %%_f7cxaqdes_obj
           |%%_f7yt8Tdes_list|)
         (|%%_f7mvJJ5es_memn|)
         (#%if (null? |%%_f7yt8Tdes_list|)
           #f
           (#%if (|%%_f7SAcZces_n|
                   (car |%%_f7yt8Tdes_list|)
                   %%_f7cxaqdes_obj)
             |%%_f7yt8Tdes_list|
             (|%%_f7mvJJ5es_memn|
               |%%_f7SAcZces_n|
               %%_f7cxaqdes_obj
               (cdr |%%_f7yt8Tdes_list|)))))))
    ()
    (#%begin
      (#%set! assq
        (#%lambda #t
          (|%%_f7IrHa6es_obj| |%%_f72oFD6es_alist|)
          (|%%_f70zLg5es_assn|)
          (|%%_f70zLg5es_assn|
            eq?
            |%%_f7IrHa6es_obj|
            |%%_f72oFD6es_alist|)))
      (#%set! assv
        (#%lambda #t
          (|%%_f7okD47es_obj| |%%_f7KgBx7es_alist|)
          (|%%_f70zLg5es_assn|)
          (|%%_f70zLg5es_assn|
            eqv?
            |%%_f7okD47es_obj|
            |%%_f7KgBx7es_alist|)))
      (#%set! assoc
        (#%lambda #t
          (%%_f74dz-7es_obj %%_f7q9xr8es_alist)
          (|%%_f70zLg5es_assn|)
          (|%%_f70zLg5es_assn|
            equal?
            %%_f74dz-7es_obj
            %%_f7q9xr8es_alist)))
      (#%set! memq
        (#%lambda #t
          (|%%_f7M5vU8es_obj| %%_f762tl9es_list)
          (|%%_f7mvJJ5es_memn|)
          (|%%_f7mvJJ5es_memn|
            eq?
            |%%_f7M5vU8es_obj|
            %%_f762tl9es_list)))
      (#%set! memv
        (#%lambda #t
          (|%%_f7s-qO9es_obj| |%%_f7OWofaes_list|)
          (|%%_f7mvJJ5es_memn|)
          (|%%_f7mvJJ5es_memn|
            eqv?
            |%%_f7s-qO9es_obj|
            |%%_f7OWofaes_list|)))
      (#%set! member
        (#%lambda #t
          (|%%_f78TmIaes_obj| |%%_f7uPk9bes_list|)
          (|%%_f7mvJJ5es_memn|)
          (|%%_f7mvJJ5es_memn|
            equal?
            |%%_f78TmIaes_obj|
            |%%_f7uPk9bes_list|))))))
(#%program
  ((cdr . 1) (car . 1) (compose2 . 1))
  ()
  (cdr car compose2)
  (#%define cadr (compose2 car cdr)))
(#%program
  ((car . 1) (cdr . 1) (compose2 . 1))
  ()
  (car cdr compose2)
  (#%define cdar (compose2 cdr car)))
(#%program
  ((cdr . 2) (compose2 . 1))
  ()
  (cdr compose2)
  (#%define cddr (compose2 cdr cdr)))
(#%program
  ((car . 2) (compose2 . 1))
  ()
  (car compose2)
  (#%define caar (compose2 car car)))
(#%program
  ((caar . 1) (car . 1) (compose2 . 1))
  ()
  (caar car compose2)
  (#%define caaar (compose2 car caar)))
(#%program
  ((cadr . 1) (car . 1) (compose2 . 1))
  ()
  (cadr car compose2)
  (#%define caadr (compose2 car cadr)))
(#%program
  ((cdar . 1) (car . 1) (compose2 . 1))
  ()
  (cdar car compose2)
  (#%define cadar (compose2 car cdar)))
(#%program
  ((cddr . 1) (car . 1) (compose2 . 1))
  ()
  (cddr car compose2)
  (#%define caddr (compose2 car cddr)))
(#%program
  ((caar . 1) (cdr . 1) (compose2 . 1))
  ()
  (caar cdr compose2)
  (#%define cdaar (compose2 cdr caar)))
(#%program
  ((cadr . 1) (cdr . 1) (compose2 . 1))
  ()
  (cadr cdr compose2)
  (#%define cdadr (compose2 cdr cadr)))
(#%program
  ((cdar . 1) (cdr . 1) (compose2 . 1))
  ()
  (cdar cdr compose2)
  (#%define cddar (compose2 cdr cdar)))
(#%program
  ((cddr . 1) (cdr . 1) (compose2 . 1))
  ()
  (cddr cdr compose2)
  (#%define cdddr (compose2 cdr cddr)))
(#%program
  ((caaar . 1) (car . 1) (compose2 . 1))
  ()
  (caaar car compose2)
  (#%define caaaar (compose2 car caaar)))
(#%program
  ((caadr . 1) (car . 1) (compose2 . 1))
  ()
  (caadr car compose2)
  (#%define caaadr (compose2 car caadr)))
(#%program
  ((cadar . 1) (car . 1) (compose2 . 1))
  ()
  (cadar car compose2)
  (#%define caadar (compose2 car cadar)))
(#%program
  ((caddr . 1) (car . 1) (compose2 . 1))
  ()
  (caddr car compose2)
  (#%define caaddr (compose2 car caddr)))
(#%program
  ((cdaar . 1) (car . 1) (compose2 . 1))
  ()
  (cdaar car compose2)
  (#%define cadaar (compose2 car cdaar)))
(#%program
  ((cdadr . 1) (car . 1) (compose2 . 1))
  ()
  (cdadr car compose2)
  (#%define cadadr (compose2 car cdadr)))
(#%program
  ((cddar . 1) (car . 1) (compose2 . 1))
  ()
  (cddar car compose2)
  (#%define caddar (compose2 car cddar)))
(#%program
  ((cdddr . 1) (car . 1) (compose2 . 1))
  ()
  (cdddr car compose2)
  (#%define cadddr (compose2 car cdddr)))
(#%program
  ((caaar . 1) (cdr . 1) (compose2 . 1))
  ()
  (caaar cdr compose2)
  (#%define cdaaar (compose2 cdr caaar)))
(#%program
  ((caadr . 1) (cdr . 1) (compose2 . 1))
  ()
  (caadr cdr compose2)
  (#%define cdaadr (compose2 cdr caadr)))
(#%program
  ((cadar . 1) (cdr . 1) (compose2 . 1))
  ()
  (cadar cdr compose2)
  (#%define cdadar (compose2 cdr cadar)))
(#%program
  ((caddr . 1) (cdr . 1) (compose2 . 1))
  ()
  (caddr cdr compose2)
  (#%define cdaddr (compose2 cdr caddr)))
(#%program
  ((cdaar . 1) (cdr . 1) (compose2 . 1))
  ()
  (cdaar cdr compose2)
  (#%define cddaar (compose2 cdr cdaar)))
(#%program
  ((cdadr . 1) (cdr . 1) (compose2 . 1))
  ()
  (cdadr cdr compose2)
  (#%define cddadr (compose2 cdr cdadr)))
(#%program
  ((cddar . 1) (cdr . 1) (compose2 . 1))
  ()
  (cddar cdr compose2)
  (#%define cdddar (compose2 cdr cddar)))
(#%program
  ((cdddr . 1) (cdr . 1) (compose2 . 1))
  ()
  (cdddr cdr compose2)
  (#%define cddddr (compose2 cdr cdddr)))
(#%program
  ((cdr . 1)
   (append2 . 1)
   (car . 1)
   (cons . 1)
   (|%%_f7em4Nees_ls2| . 2)
   (|%%_f7Up6kees_ls1| . 3)
   (null? . 1))
  ()
  (cons car cdr append2 null?)
  (#%define append2
    (#%lambda #t
      (|%%_f7Up6kees_ls1| |%%_f7em4Nees_ls2|)
      ()
      (#%if (null? |%%_f7Up6kees_ls1|)
        |%%_f7em4Nees_ls2|
        (cons (car |%%_f7Up6kees_ls1|)
              (append2
                (cdr |%%_f7Up6kees_ls1|)
                |%%_f7em4Nees_ls2|))))))
(#%program
  ((append2 . 1))
  ()
  (append2)
  (#%define append append2))
(#%program
  ((|%%_f7We0Hfes_base-case| . 1)
   (|%%_f7C7YAges_args| . 3)
   (cdr . 2)
   (car . 2)
   (|%%_f7Ai2efes_proc| . 1)
   (%%_f7gb-7ges_helper . 2)
   (|%%_f7Y3W1hes_acc| . 2)
   (|%%_f7i0Uuhes_argls| . 3)
   (null? . 2))
  ((%%_f7gb-7ges_helper . 1))
  (null? cdr car)
  (#%define _make-left-pairwise-nary
    (#%lambda #t
      (|%%_f7Ai2efes_proc| |%%_f7We0Hfes_base-case|)
      ()
      (#%letrec #t
        ((%%_f7gb-7ges_helper
           (#%lambda #t
             (|%%_f7Y3W1hes_acc| |%%_f7i0Uuhes_argls|)
             (%%_f7gb-7ges_helper |%%_f7Ai2efes_proc|)
             (#%if (null? |%%_f7i0Uuhes_argls|)
               |%%_f7Y3W1hes_acc|
               (%%_f7gb-7ges_helper
                 (|%%_f7Ai2efes_proc|
                   |%%_f7Y3W1hes_acc|
                   (car |%%_f7i0Uuhes_argls|))
                 (cdr |%%_f7i0Uuhes_argls|))))))
        (|%%_f7We0Hfes_base-case| |%%_f7Ai2efes_proc|)
        (#%lambda #t
          |%%_f7C7YAges_args|
          (%%_f7gb-7ges_helper |%%_f7We0Hfes_base-case|)
          (#%if (null? |%%_f7C7YAges_args|)
            |%%_f7We0Hfes_base-case|
            (%%_f7gb-7ges_helper
              (car |%%_f7C7YAges_args|)
              (cdr |%%_f7C7YAges_args|))))))))
(#%program
  ((length . 1)
   (make-string . 1)
   (|%%_f7-UPoies_l| . 2)
   (+ . 1)
   (cdr . 1)
   (|%%_f7EYRXhes_l2s| . 2)
   (car . 1)
   (|%%_f70KJLjes_n| . 2)
   (string-set! . 1)
   (|%%_f7GNLijes_s| . 3)
   (|%%_f7kRNRies_l| . 3)
   (null? . 1))
  ((|%%_f7EYRXhes_l2s| . 1))
  (make-string length car string-set! + cdr null?)
  (#%define list->string
    (#%letrec #t
      ((|%%_f7EYRXhes_l2s|
         (#%lambda #t
           (|%%_f7kRNRies_l|
             |%%_f7GNLijes_s|
             |%%_f70KJLjes_n|)
           (|%%_f7EYRXhes_l2s|)
           (#%if (null? |%%_f7kRNRies_l|)
             |%%_f7GNLijes_s|
             (#%begin
               (string-set!
                 |%%_f7GNLijes_s|
                 |%%_f70KJLjes_n|
                 (car |%%_f7kRNRies_l|))
               (|%%_f7EYRXhes_l2s|
                 (cdr |%%_f7kRNRies_l|)
                 |%%_f7GNLijes_s|
                 (+ |%%_f70KJLjes_n| 1)))))))
      ()
      (#%lambda #t
        (|%%_f7-UPoies_l|)
        (|%%_f7EYRXhes_l2s|)
        (|%%_f7EYRXhes_l2s|
          |%%_f7-UPoies_l|
          (make-string (length |%%_f7-UPoies_l|))
          0)))))
(#%program
  ((string-length . 1)
   (|%%_f7ICFFkes_s| . 2)
   (- . 2)
   (string-ref . 1)
   (cons . 1)
   (|%%_f72zD6les_s| . 2)
   (|%%_f7mGHckes_s2l| . 2)
   (|%%_f7ovBzles_h| . 2)
   (|%%_f7Krz0mes_n| . 3)
   (< . 1))
  ((|%%_f7mGHckes_s2l| . 1))
  (string-length string-ref cons - <)
  (#%define string->list
    (#%letrec #t
      ((|%%_f7mGHckes_s2l|
         (#%lambda #t
           (|%%_f72zD6les_s|
             |%%_f7ovBzles_h|
             |%%_f7Krz0mes_n|)
           (|%%_f7mGHckes_s2l|)
           (#%if (< |%%_f7Krz0mes_n| 0)
             |%%_f7ovBzles_h|
             (|%%_f7mGHckes_s2l|
               |%%_f72zD6les_s|
               (cons (string-ref |%%_f72zD6les_s| |%%_f7Krz0mes_n|)
                     |%%_f7ovBzles_h|)
               (- |%%_f7Krz0mes_n| 1))))))
      ()
      (#%lambda #t
        (|%%_f7ICFFkes_s|)
        (|%%_f7mGHckes_s2l|)
        (|%%_f7mGHckes_s2l|
          |%%_f7ICFFkes_s|
          ()
          (- (string-length |%%_f7ICFFkes_s|) 1))))))
(#%program
  ((%%_f74oxtmes_elems . 1) (list->vector . 1))
  ()
  (list->vector)
  (#%define vector
    (#%lambda #t
      %%_f74oxtmes_elems
      ()
      (list->vector %%_f74oxtmes_elems))))
(#%program
  ((|%%_f7qkvWmes_elems| . 1) (list->string . 1))
  ()
  (list->string)
  (#%define string
    (#%lambda #t
      |%%_f7qkvWmes_elems|
      ()
      (list->string |%%_f7qkvWmes_elems|))))
(#%program
  ((_make-parameter . 1))
  ()
  (_make-parameter)
  (#%define current-url (_make-parameter "file:.")))
(#%program
  ((car . 1)
   (string-length . 1)
   (string-append . 1)
   (%%_f7s9phoes_l . 1)
   (- . 1)
   (|%%_f76drQnes_v| . 4)
   (string-ref . 1)
   (eqv? . 1)
   (current-url . 3)
   (normalize-url . 2)
   (|%%_f7Mgtnnes_rest| . 2)
   (null? . 1))
  ()
  (string-length
    string-ref
    -
    eqv?
    string-append
    car
    normalize-url
    current-url
    null?)
  (#%define current-directory
    (#%lambda #t
      |%%_f7Mgtnnes_rest|
      ()
      (#%if (null? |%%_f7Mgtnnes_rest|)
        (normalize-url (current-url) ".")
        (current-url
          (normalize-url
            (current-url)
            ((#%lambda #t
               (|%%_f76drQnes_v|)
               ()
               ((#%lambda #t
                  (%%_f7s9phoes_l)
                  (|%%_f76drQnes_v|)
                  (#%if (eqv? (string-ref
                                |%%_f76drQnes_v|
                                (- %%_f7s9phoes_l 1))
                              #\/)
                    |%%_f76drQnes_v|
                    (string-append |%%_f76drQnes_v| "/")))
                (string-length |%%_f76drQnes_v|)))
             (car |%%_f7Mgtnnes_rest|))))))))
(#%program
  ((void . 1))
  ()
  (void)
  (#%define file-handler (void)))
(#%program
  ((void . 1))
  ()
  (void)
  (#%define add-file-handler (void)))
(#%program
  ((load . 1)
   (|%%_f7aTeyqes_extension| . 1)
   (string-downcase . 1)
   (string->symbol . 1)
   (|%%_f7QWg5qes__load| . 1)
   (cdr . 1)
   (|%%_f7wPc_qes_t| . 2)
   (file-handler . 1)
   (|%%_f7u-iEpes_thunk| . 1)
   (cons . 2)
   (|%%_f7O5nKoes_*file-handlers*| . 4)
   (%%_f782lbpes_extension . 2)
   (assq . 2)
   (not . 1)
   (add-file-handler . 1))
  ((file-handler . 1)
   (|%%_f7O5nKoes_*file-handlers*| . 1)
   (add-file-handler . 1))
  (cdr string->symbol
       string-downcase
       load
       file-handler
       cons
       not
       assq
       add-file-handler)
  ((#%lambda #t
     (|%%_f7O5nKoes_*file-handlers*|)
     ()
     (#%begin
       (#%set! add-file-handler
         (#%lambda #t
           (%%_f782lbpes_extension |%%_f7u-iEpes_thunk|)
           (|%%_f7O5nKoes_*file-handlers*|)
           (#%if (not (assq %%_f782lbpes_extension
                            |%%_f7O5nKoes_*file-handlers*|))
             (#%set! |%%_f7O5nKoes_*file-handlers*|
               (cons (cons %%_f782lbpes_extension
                           |%%_f7u-iEpes_thunk|)
                     |%%_f7O5nKoes_*file-handlers*|))
             #!void)))
       (#%set! file-handler
         ((#%lambda #t
            (|%%_f7QWg5qes__load|)
            (|%%_f7O5nKoes_*file-handlers*|)
            (#%lambda #t
              (|%%_f7aTeyqes_extension|)
              (|%%_f7QWg5qes__load|
                |%%_f7O5nKoes_*file-handlers*|)
              ((#%lambda #t
                 (|%%_f7wPc_qes_t|)
                 (|%%_f7QWg5qes__load|)
                 (#%if |%%_f7wPc_qes_t|
                   (cdr |%%_f7wPc_qes_t|)
                   |%%_f7QWg5qes__load|))
               (assq (string->symbol
                       (string-downcase |%%_f7aTeyqes_extension|))
                     |%%_f7O5nKoes_*file-handlers*|))))
          load))))
   ()))
(#%program
  ((|%%_f7cI8Vres_rest| . 1)
   (|%%_f7yE6mses_file| . 1)
   (current-url . 1)
   (normalize-url . 1)
   (|%%_f7SLasres_proc| . 1)
   (apply . 1))
  ()
  (apply current-url normalize-url)
  (#%define make-io-proc
    (#%lambda #t
      (|%%_f7SLasres_proc|)
      ()
      (#%lambda #t
        (|%%_f7yE6mses_file| . |%%_f7cI8Vres_rest|)
        (|%%_f7SLasres_proc|)
        (apply |%%_f7SLasres_proc|
               (normalize-url (current-url) |%%_f7yE6mses_file|)
               |%%_f7cI8Vres_rest|)))))
(#%program
  ((|%%_f7ibSZves_url| . 1)
   (string->list . 1)
   (reverse! . 1)
   (cons . 1)
   (cdr . 1)
   (|%%_f7E7Qqwes_loop| . 2)
   (|%%_f7k0Mkxes_acc| . 2)
   (list->string . 1)
   (car . 2)
   (equal? . 1)
   (|%%_f7-3OTwes_x| . 4)
   (null? . 1)
   (void . 1)
   (|%%_f7UA4Pses_file-extension| . 1)
   (|%%_f7YeUwves_fe| . 2)
   (file-handler . 1)
   (|%%_f7gmYCues_e| . 1)
   (|%%_f7Wp-9ues_m| . 1)
   (|%%_f7CiW3ves_fk| . 1)
   (call-with-failure-continuation . 1)
   (with-failure-continuation . 1)
   (%%_f7ex2gtes_file . 1)
   (|%%_f7At0Jtes_previous-url| . 3)
   (normalize-url . 1)
   (current-url . 6)
   (load . 1)
   (open-output-file . 2)
   (open-source-input-file . 2)
   (make-io-proc . 3)
   (open-input-file . 2))
  ((|%%_f7E7Qqwes_loop| . 1)
   (load . 1)
   (open-output-file . 1)
   (open-source-input-file . 1)
   (open-input-file . 1))
  (open-input-file
    open-input-file
    make-io-proc
    open-source-input-file
    open-source-input-file
    open-output-file
    open-output-file
    load
    normalize-url
    current-url
    file-handler
    call-with-failure-continuation
    with-failure-continuation
    void
    reverse!
    string->list
    null?
    cdr
    cons
    list->string
    equal?
    car)
  ((#%lambda #t
     (|%%_f7UA4Pses_file-extension|)
     ()
     (#%begin
       (#%set! open-input-file
         (make-io-proc open-input-file))
       (#%set! open-source-input-file
         (make-io-proc open-source-input-file))
       (#%set! open-output-file
         (make-io-proc open-output-file))
       (#%set! load
         (#%lambda #t
           (%%_f7ex2gtes_file)
           (|%%_f7UA4Pses_file-extension|)
           (#%begin
             ((#%lambda #t
                (|%%_f7At0Jtes_previous-url|)
                (%%_f7ex2gtes_file |%%_f7UA4Pses_file-extension|)
                (#%begin
                  (current-url
                    (normalize-url
                      |%%_f7At0Jtes_previous-url|
                      %%_f7ex2gtes_file))
                  (with-failure-continuation
                    (#%lambda #t
                      (|%%_f7Wp-9ues_m| |%%_f7gmYCues_e|)
                      (|%%_f7At0Jtes_previous-url|)
                      (#%begin
                        (current-url |%%_f7At0Jtes_previous-url|)
                        (call-with-failure-continuation
                          (#%lambda #t
                            (|%%_f7CiW3ves_fk|)
                            (|%%_f7gmYCues_e| |%%_f7Wp-9ues_m|)
                            (|%%_f7CiW3ves_fk|
                              |%%_f7Wp-9ues_m|
                              |%%_f7gmYCues_e|)))))
                    (#%lambda #t
                      ()
                      (|%%_f7UA4Pses_file-extension|)
                      ((#%lambda #t
                         (|%%_f7YeUwves_fe|)
                         ()
                         ((file-handler
                            (#%if |%%_f7YeUwves_fe| |%%_f7YeUwves_fe| "scm"))
                          (current-url)))
                       (|%%_f7UA4Pses_file-extension| (current-url)))))
                  (current-url |%%_f7At0Jtes_previous-url|)))
              (current-url))
             (void))))))
   (#%lambda #t
     (|%%_f7ibSZves_url|)
     ()
     ((#%letrec #t
        ((|%%_f7E7Qqwes_loop|
           (#%lambda #t
             (|%%_f7-3OTwes_x| |%%_f7k0Mkxes_acc|)
             (|%%_f7E7Qqwes_loop|)
             (#%if (null? |%%_f7-3OTwes_x|)
               #f
               (#%if (equal? (car |%%_f7-3OTwes_x|) #\.)
                 (list->string |%%_f7k0Mkxes_acc|)
                 (|%%_f7E7Qqwes_loop|
                   (cdr |%%_f7-3OTwes_x|)
                   (cons (car |%%_f7-3OTwes_x|) |%%_f7k0Mkxes_acc|)))))))
        ()
        |%%_f7E7Qqwes_loop|)
      (reverse! (string->list |%%_f7ibSZves_url|))
      ()))))
(#%program
  ((|%%_f7GYJNxes_str| . 1)
   (load-native-library . 1)
   (native-library-binding-names . 1)
   (|%%_f7mRFHyes_binding-names| . 1)
   (|%%_f70VHeyes_nl| . 2)
   (native-library-binding . 1)
   (|%%_f7IND8zes_name| . 2)
   (putprop . 1)
   (for-each . 1))
  ()
  (load-native-library
    native-library-binding
    putprop
    for-each
    native-library-binding-names)
  (#%define load-module
    (#%lambda #t
      (|%%_f7GYJNxes_str|)
      ()
      ((#%lambda #t
         (|%%_f70VHeyes_nl|)
         ()
         ((#%lambda #t
            (|%%_f7mRFHyes_binding-names|)
            (|%%_f70VHeyes_nl|)
            (for-each
              (#%lambda #t
                (|%%_f7IND8zes_name|)
                (|%%_f70VHeyes_nl|)
                (putprop
                  |%%_f7IND8zes_name|
                  (native-library-binding
                    |%%_f70VHeyes_nl|
                    |%%_f7IND8zes_name|)))
              |%%_f7mRFHyes_binding-names|))
          (native-library-binding-names |%%_f70VHeyes_nl|)))
       (load-native-library |%%_f7GYJNxes_str|)))))
(#%program
  ((append2 . 1) (_make-left-pairwise-nary . 1))
  ()
  (append2 _make-left-pairwise-nary)
  (#%define append
    (_make-left-pairwise-nary append2 ())))
(#%program
  ((|%%_f72KBBzes_x| . 2)
   (null? . 2)
   (|%%_f74zvYAes_lag| . 1)
   (cdr . 3)
   (|%%_f7oGz2Aes_lp| . 2)
   (|%%_f76opjCes_lag| . 2)
   (|%%_f7MrrSBes_x| . 2)
   (eq? . 1)
   (not . 1)
   (|%%_f7qvtpBes_x| . 3)
   (|%%_f7KCxvAes_x| . 3)
   (pair? . 2))
  ((|%%_f7oGz2Aes_lp| . 1))
  (pair? cdr eq? not null?)
  (#%define proper-list?
    (#%lambda #t
      (|%%_f72KBBzes_x|)
      ()
      ((#%letrec #t
         ((|%%_f7oGz2Aes_lp|
            (#%lambda #t
              (|%%_f7KCxvAes_x| |%%_f74zvYAes_lag|)
              (|%%_f7oGz2Aes_lp|)
              (#%if (pair? |%%_f7KCxvAes_x|)
                ((#%lambda #t
                   (|%%_f7qvtpBes_x|)
                   (|%%_f74zvYAes_lag| |%%_f7oGz2Aes_lp|)
                   (#%if (pair? |%%_f7qvtpBes_x|)
                     ((#%lambda #t
                        (|%%_f7MrrSBes_x| |%%_f76opjCes_lag|)
                        (|%%_f7oGz2Aes_lp|)
                        (#%if (not (eq? |%%_f7MrrSBes_x| |%%_f76opjCes_lag|))
                          (|%%_f7oGz2Aes_lp|
                            |%%_f7MrrSBes_x|
                            |%%_f76opjCes_lag|)
                          #f))
                      (cdr |%%_f7qvtpBes_x|)
                      (cdr |%%_f74zvYAes_lag|))
                     (null? |%%_f7qvtpBes_x|)))
                 (cdr |%%_f7KCxvAes_x|))
                (null? |%%_f7KCxvAes_x|)))))
         ()
         |%%_f7oGz2Aes_lp|)
       |%%_f72KBBzes_x|
       |%%_f72KBBzes_x|))))
(#%program
  ((proper-list? . 1))
  ()
  (proper-list?)
  (#%define list? proper-list?))
(#%program
  ((|%%_f7sknMCes_general-expt| . 1)
   (|%%_f78djGDes_integer-expt| . 1)
   (denominator . 1)
   (numerator . 1)
   (|%%_f7OgldDes_rational-expt| . 1)
   (integer? . 2)
   (not . 1)
   (rational? . 1)
   (|%%_f7u9h7Ees_base| . 9)
   (|%%_f7Q5fAEes_exponent| . 8)
   (|%%_f7CtUyJes_squaring| . 3)
   (odd? . 1)
   (quotient . 1)
   (|%%_f7AE-bIes_loop| . 2)
   (|%%_f7gxW5Jes_result| . 3)
   (|%%_f7WAYEIes_rest| . 3)
   (zero? . 3)
   (abs . 2)
   (ashl . 2)
   (|%%_f7eI0LHes_exponent| . 7)
   (negative? . 3)
   (= . 1)
   (|%%_f7UL2iHes_base| . 4)
   (exact? . 5)
   (|%%_f7cT6oGes_base-denominator| . 1)
   (|%%_f7yP4RGes_exponent| . 2)
   (|%%_f7SW8XFes_base-numerator| . 1)
   (expt . 2)
   (/ . 3)
   (|%%_f7a2d1Fes_base| . 1)
   (log . 1)
   (|%%_f7w-auFes_exponent| . 1)
   (* . 3)
   (exp . 1))
  ((|%%_f7AE-bIes_loop| . 1)
   (|%%_f78djGDes_integer-expt| . 1)
   (|%%_f7OgldDes_rational-expt| . 1)
   (|%%_f7sknMCes_general-expt| . 1))
  (numerator
    denominator
    not
    integer?
    rational?
    quotient
    odd?
    zero?
    negative?
    ashl
    abs
    exact?
    =
    expt
    /
    *
    log
    exp)
  (#%define expt
    (#%letrec #t
      ((|%%_f7sknMCes_general-expt|
         (#%lambda #t
           (|%%_f7a2d1Fes_base| |%%_f7w-auFes_exponent|)
           ()
           (exp (* |%%_f7w-auFes_exponent|
                   (log |%%_f7a2d1Fes_base|)))))
       (|%%_f7OgldDes_rational-expt|
         (#%lambda #t
           (|%%_f7SW8XFes_base-numerator|
             |%%_f7cT6oGes_base-denominator|
             |%%_f7yP4RGes_exponent|)
           ()
           (/ (expt |%%_f7SW8XFes_base-numerator|
                    |%%_f7yP4RGes_exponent|)
              (expt |%%_f7cT6oGes_base-denominator|
                    |%%_f7yP4RGes_exponent|))))
       (|%%_f78djGDes_integer-expt|
         (#%lambda #t
           (|%%_f7UL2iHes_base| |%%_f7eI0LHes_exponent|)
           ()
           (#%if (#%if (exact? |%%_f7UL2iHes_base|)
                   (= |%%_f7UL2iHes_base| 2)
                   #f)
             (#%if (negative? |%%_f7eI0LHes_exponent|)
               (/ (ashl 1 (abs |%%_f7eI0LHes_exponent|)))
               (ashl 1 |%%_f7eI0LHes_exponent|))
             ((#%letrec #t
                ((|%%_f7AE-bIes_loop|
                   (#%lambda #t
                     (|%%_f7WAYEIes_rest|
                       |%%_f7gxW5Jes_result|
                       |%%_f7CtUyJes_squaring|)
                     (|%%_f7AE-bIes_loop|)
                     (#%if (zero? |%%_f7WAYEIes_rest|)
                       |%%_f7gxW5Jes_result|
                       (|%%_f7AE-bIes_loop|
                         (quotient |%%_f7WAYEIes_rest| 2)
                         (#%if (odd? |%%_f7WAYEIes_rest|)
                           (* |%%_f7gxW5Jes_result| |%%_f7CtUyJes_squaring|)
                           |%%_f7gxW5Jes_result|)
                         (* |%%_f7CtUyJes_squaring|
                            |%%_f7CtUyJes_squaring|))))))
                ()
                |%%_f7AE-bIes_loop|)
              (#%if (negative? |%%_f7eI0LHes_exponent|)
                (abs |%%_f7eI0LHes_exponent|)
                |%%_f7eI0LHes_exponent|)
              1
              (#%if (negative? |%%_f7eI0LHes_exponent|)
                (/ |%%_f7UL2iHes_base|)
                |%%_f7UL2iHes_base|))))))
      ()
      (#%lambda #t
        (|%%_f7u9h7Ees_base| |%%_f7Q5fAEes_exponent|)
        (|%%_f78djGDes_integer-expt|
          |%%_f7OgldDes_rational-expt|
          |%%_f7sknMCes_general-expt|)
        (#%if (zero? |%%_f7Q5fAEes_exponent|)
          (#%if (exact? |%%_f7Q5fAEes_exponent|) 1 1.0)
          (#%if (zero? |%%_f7u9h7Ees_base|)
            (#%if (exact? |%%_f7Q5fAEes_exponent|)
              |%%_f7u9h7Ees_base|
              0.0)
            (#%if (#%if (exact? |%%_f7u9h7Ees_base|)
                    (#%if (rational? |%%_f7u9h7Ees_base|)
                      (not (integer? |%%_f7u9h7Ees_base|))
                      #f)
                    #f)
              (|%%_f7OgldDes_rational-expt|
                (numerator |%%_f7u9h7Ees_base|)
                (denominator |%%_f7u9h7Ees_base|)
                |%%_f7Q5fAEes_exponent|)
              (#%if (#%if (exact? |%%_f7Q5fAEes_exponent|)
                      (integer? |%%_f7Q5fAEes_exponent|)
                      #f)
                (|%%_f78djGDes_integer-expt|
                  |%%_f7u9h7Ees_base|
                  |%%_f7Q5fAEes_exponent|)
                (|%%_f7sknMCes_general-expt|
                  |%%_f7u9h7Ees_base|
                  |%%_f7Q5fAEes_exponent|)))))))))
(#%program
  ((- . 1)
   (|%%_f7-eMmLes_tmp| . 2)
   (/ . 2)
   (modpow . 2)
   (|%%_f7kbKPLes_tmp| . 2)
   (* . 3)
   (even? . 1)
   (|%%_f7EiOVKes_n| . 6)
   (|%%_f7YpS_Jes_x| . 4)
   (modulo . 4)
   (|%%_f7imQsKes_y| . 4)
   (= . 1))
  ()
  (even? modpow / * - modulo =)
  (#%define modpow
    (#%lambda #t
      (|%%_f7YpS_Jes_x|
        |%%_f7imQsKes_y|
        |%%_f7EiOVKes_n|)
      ()
      (#%if (= |%%_f7imQsKes_y| 1)
        (modulo |%%_f7YpS_Jes_x| |%%_f7EiOVKes_n|)
        (#%if (even? |%%_f7imQsKes_y|)
          ((#%lambda #t
             (|%%_f7kbKPLes_tmp|)
             (|%%_f7EiOVKes_n|)
             (modulo
               (* |%%_f7kbKPLes_tmp| |%%_f7kbKPLes_tmp|)
               |%%_f7EiOVKes_n|))
           (modpow
             |%%_f7YpS_Jes_x|
             (/ |%%_f7imQsKes_y| 2)
             |%%_f7EiOVKes_n|))
          ((#%lambda #t
             (|%%_f7-eMmLes_tmp|)
             (|%%_f7EiOVKes_n| |%%_f7YpS_Jes_x|)
             (modulo
               (* |%%_f7YpS_Jes_x|
                  (modulo
                    (* |%%_f7-eMmLes_tmp| |%%_f7-eMmLes_tmp|)
                    |%%_f7EiOVKes_n|))
               |%%_f7EiOVKes_n|))
           (modpow
             |%%_f7YpS_Jes_x|
             (/ (- |%%_f7imQsKes_y| 1) 2)
             |%%_f7EiOVKes_n|)))))))
(#%program
  ((round . 1)
   (= . 1)
   (real? . 1)
   (|%%_f7G7IgMes_n| . 4)
   (_integer? . 1))
  ()
  (real? round = _integer?)
  (#%define integer?
    (#%lambda #t
      (|%%_f7G7IgMes_n|)
      ()
      (#%if (_integer? |%%_f7G7IgMes_n|)
        #t
        (#%if (real? |%%_f7G7IgMes_n|)
          (= (round |%%_f7G7IgMes_n|) |%%_f7G7IgMes_n|)
          #f)))))
(#%program
  ((complex? . 1)
   (|%%_f704GJMes_oldcomp?| . 1)
   (not . 1)
   (|%%_f7m0EaNes_n| . 2)
   (number? . 1))
  ()
  (complex? number? not)
  (#%define real?
    ((#%lambda #t
       (|%%_f704GJMes_oldcomp?|)
       ()
       (#%lambda #t
         (|%%_f7m0EaNes_n|)
         (|%%_f704GJMes_oldcomp?|)
         (#%if (number? |%%_f7m0EaNes_n|)
           (not (|%%_f704GJMes_oldcomp?| |%%_f7m0EaNes_n|))
           #f)))
     complex?)))
(#%program
  ((real? . 1))
  ()
  (real?)
  (#%define rational? real?))
(#%program
  ((number? . 1))
  ()
  (number?)
  (#%define complex? number?))
(#%program
  ((- . 1)
   (< . 1)
   (imag-part . 1)
   (real-part . 1)
   (|%%_f7oRxxOes_b| . 2)
   (|%%_f72Vz4Oes_a| . 2)
   (* . 2)
   (+ . 1)
   (sqrt . 1)
   (|%%_f7IYBDNes_num| . 6)
   (real? . 1)
   (not . 1))
  ()
  (< - + * sqrt real-part imag-part not real?)
  (#%define abs
    (#%lambda #t
      (|%%_f7IYBDNes_num|)
      ()
      (#%if (not (real? |%%_f7IYBDNes_num|))
        ((#%lambda #t
           (|%%_f72Vz4Oes_a| |%%_f7oRxxOes_b|)
           ()
           (sqrt (+ (* |%%_f72Vz4Oes_a| |%%_f72Vz4Oes_a|)
                    (* |%%_f7oRxxOes_b| |%%_f7oRxxOes_b|))))
         (real-part |%%_f7IYBDNes_num|)
         (imag-part |%%_f7IYBDNes_num|))
        (#%if (< |%%_f7IYBDNes_num| 0)
          (- |%%_f7IYBDNes_num|)
          |%%_f7IYBDNes_num|)))))
(#%program
  ((void . 1))
  ()
  (void)
  (#%define min (void)))
(#%program
  ((void . 1))
  ()
  (void)
  (#%define max (void)))
(#%program
  ((> . 1)
   (|%%_f76znOQes_x1| . 3)
   (|%%_f7MCplQes_args| . 2)
   (max . 1)
   (< . 1)
   (|%%_f7qGrUPes_x1| . 3)
   (|%%_f74KtrPes_args| . 2)
   (min . 1)
   (inexact? . 3)
   (cdr . 2)
   (|%%_f7KNv-Oes__min_max| . 4)
   (car . 3)
   (|%%_f7svlfRes_proc| . 3)
   (exact->inexact . 1)
   (|%%_f7OrjIRes_mv| . 5)
   (exact? . 1)
   (|%%_f7ukfCSes_inexact| . 3)
   (|%%_f78oh9Ses_args| . 6)
   (null? . 3))
  ((max . 1)
   (min . 1)
   (|%%_f7KNv-Oes__min_max| . 1))
  (min <
       max
       >
       car
       inexact?
       cdr
       exact?
       exact->inexact
       null?)
  (#%letrec #t
    ((|%%_f7KNv-Oes__min_max|
       (#%lambda #t
         (|%%_f7svlfRes_proc|
           |%%_f7OrjIRes_mv|
           |%%_f78oh9Ses_args|
           |%%_f7ukfCSes_inexact|)
         (|%%_f7KNv-Oes__min_max|)
         (#%if (null? |%%_f78oh9Ses_args|)
           (#%if (#%if |%%_f7ukfCSes_inexact|
                   (exact? |%%_f7OrjIRes_mv|)
                   #f)
             (exact->inexact |%%_f7OrjIRes_mv|)
             |%%_f7OrjIRes_mv|)
           (#%if (|%%_f7svlfRes_proc|
                   (car |%%_f78oh9Ses_args|)
                   |%%_f7OrjIRes_mv|)
             (|%%_f7KNv-Oes__min_max|
               |%%_f7svlfRes_proc|
               (car |%%_f78oh9Ses_args|)
               (cdr |%%_f78oh9Ses_args|)
               (#%if |%%_f7ukfCSes_inexact|
                 #t
                 (inexact? (car |%%_f78oh9Ses_args|))))
             (|%%_f7KNv-Oes__min_max|
               |%%_f7svlfRes_proc|
               |%%_f7OrjIRes_mv|
               (cdr |%%_f78oh9Ses_args|)
               |%%_f7ukfCSes_inexact|))))))
    ()
    (#%begin
      (#%set! min
        (#%lambda #t
          (|%%_f7qGrUPes_x1| . |%%_f74KtrPes_args|)
          (|%%_f7KNv-Oes__min_max|)
          (#%if (null? |%%_f74KtrPes_args|)
            |%%_f7qGrUPes_x1|
            (|%%_f7KNv-Oes__min_max|
              <
              |%%_f7qGrUPes_x1|
              |%%_f74KtrPes_args|
              (inexact? |%%_f7qGrUPes_x1|)))))
      (#%set! max
        (#%lambda #t
          (|%%_f76znOQes_x1| . |%%_f7MCplQes_args|)
          (|%%_f7KNv-Oes__min_max|)
          (#%if (null? |%%_f7MCplQes_args|)
            |%%_f76znOQes_x1|
            (|%%_f7KNv-Oes__min_max|
              >
              |%%_f76znOQes_x1|
              |%%_f7MCplQes_args|
              (inexact? |%%_f76znOQes_x1|))))))))
(#%program
  ((|%%_f7Qgd3Tes_n| . 1) (< . 1))
  ()
  (<)
  (#%define negative?
    (#%lambda #t
      (|%%_f7Qgd3Tes_n|)
      ()
      (< |%%_f7Qgd3Tes_n| 0))))
(#%program
  ((|%%_f7adbwTes_n| . 1) (> . 1))
  ()
  (>)
  (#%define positive?
    (#%lambda #t
      (|%%_f7adbwTes_n|)
      ()
      (> |%%_f7adbwTes_n| 0))))
(#%program
  ((|%%_f7w99ZTes_n| . 1) (modulo . 1) (= . 1))
  ()
  (modulo =)
  (#%define even?
    (#%lambda #t
      (|%%_f7w99ZTes_n|)
      ()
      (= 0 (modulo |%%_f7w99ZTes_n| 2)))))
(#%program
  ((|%%_f7S57qUes_n| . 1) (even? . 1) (not . 1))
  ()
  (even? not)
  (#%define odd?
    (#%lambda #t
      (|%%_f7S57qUes_n|)
      ()
      (not (even? |%%_f7S57qUes_n|)))))
(#%program
  ((|%%_f7c25TUes_n| . 1) (= . 1))
  ()
  (=)
  (#%define zero?
    (#%lambda #t
      (|%%_f7c25TUes_n|)
      ()
      (= |%%_f7c25TUes_n| 0))))
(#%program
  ((|%%_f7y-2kVes_n| . 1) (+ . 1))
  ()
  (+)
  (#%define add1
    (#%lambda #t
      (|%%_f7y-2kVes_n|)
      ()
      (+ |%%_f7y-2kVes_n| 1))))
(#%program
  ((|%%_f7UW0NVes_n| . 1) (- . 1))
  ()
  (-)
  (#%define sub1
    (#%lambda #t
      (|%%_f7UW0NVes_n|)
      ()
      (- |%%_f7UW0NVes_n| 1))))
(#%program
  ((void . 1))
  ()
  (void)
  (#%define >= (void)))
(#%program
  ((void . 1))
  ()
  (void)
  (#%define <= (void)))
(#%program
  ((|%%_f7I7A60fs_y| . 1)
   (|%%_f7mbCF_es_x| . 1)
   (|%%_f7kmIi-es_args| . 1)
   (|%%_f7GiGL-es_loop| . 2)
   (cadr . 1)
   (car . 1)
   (|%%_f7ixOXYes_comparator| . 1)
   (|%%_f7EtMoZes_chainer| . 1)
   (cdr . 2)
   (|%%_f7-pKRZes_endstate| . 2)
   (|%%_f70fEc_es_x| . 5)
   (null? . 2)
   (|%%_f7YAQuYes_b| . 2)
   (|%%_f7CES1Yes_a| . 2)
   (> . 1)
   (>= . 1)
   (|%%_f7APYGWes__and2| . 2)
   (= . 2)
   (|%%_f7gIUAXes_b| . 2)
   (|%%_f7WLW7Xes_a| . 2)
   (< . 1)
   (|%%_f7eT-dWes__comp_help| . 2)
   (<= . 1))
  ((|%%_f7GiGL-es_loop| . 1) (>= . 1) (<= . 1))
  (null? cadr car cdr <= = < >= >)
  ((#%lambda #t
     (|%%_f7eT-dWes__comp_help| |%%_f7APYGWes__and2|)
     ()
     (#%begin
       (#%set! <=
         (|%%_f7eT-dWes__comp_help|
           (#%lambda #t
             (|%%_f7WLW7Xes_a| |%%_f7gIUAXes_b|)
             ()
             (#%if (< |%%_f7WLW7Xes_a| |%%_f7gIUAXes_b|)
               #t
               (= |%%_f7WLW7Xes_a| |%%_f7gIUAXes_b|)))
           |%%_f7APYGWes__and2|
           #t))
       (#%set! >=
         (|%%_f7eT-dWes__comp_help|
           (#%lambda #t
             (|%%_f7CES1Yes_a| |%%_f7YAQuYes_b|)
             ()
             (#%if (> |%%_f7CES1Yes_a| |%%_f7YAQuYes_b|)
               #t
               (= |%%_f7CES1Yes_a| |%%_f7YAQuYes_b|)))
           |%%_f7APYGWes__and2|
           #t))))
   (#%lambda #t
     (|%%_f7ixOXYes_comparator|
       |%%_f7EtMoZes_chainer|
       |%%_f7-pKRZes_endstate|)
     ()
     (#%lambda #t
       |%%_f7kmIi-es_args|
       (|%%_f7-pKRZes_endstate|
         |%%_f7EtMoZes_chainer|
         |%%_f7ixOXYes_comparator|)
       ((#%letrec #t
          ((|%%_f7GiGL-es_loop|
             (#%lambda #t
               (|%%_f70fEc_es_x|)
               (|%%_f7GiGL-es_loop|
                 |%%_f7-pKRZes_endstate|
                 |%%_f7EtMoZes_chainer|
                 |%%_f7ixOXYes_comparator|)
               (#%if (null? |%%_f70fEc_es_x|)
                 |%%_f7-pKRZes_endstate|
                 (#%if (null? (cdr |%%_f70fEc_es_x|))
                   |%%_f7-pKRZes_endstate|
                   (|%%_f7EtMoZes_chainer|
                     (|%%_f7ixOXYes_comparator|
                       (car |%%_f70fEc_es_x|)
                       (cadr |%%_f70fEc_es_x|))
                     (|%%_f7GiGL-es_loop| (cdr |%%_f70fEc_es_x|))))))))
          (|%%_f7-pKRZes_endstate|
            |%%_f7EtMoZes_chainer|
            |%%_f7ixOXYes_comparator|)
          |%%_f7GiGL-es_loop|)
        |%%_f7kmIi-es_args|)))
   (#%lambda #t
     (|%%_f7mbCF_es_x| |%%_f7I7A60fs_y|)
     ()
     (#%if |%%_f7mbCF_es_x| |%%_f7I7A60fs_y| #f))))
(#%program
  ((|%%_f7KYtt1fs_chainer| . 1)
   (apply . 1)
   (%%_f7o0w01fs_comparator . 1)
   (cadr . 2)
   (car . 2)
   (= . 1)
   (cdr . 2)
   (|%%_f74VrW1fs_args| . 7)
   (null? . 2)
   (< . 1)
   (<= . 2)
   (> . 1)
   (%%_f724yz0fs__?= . 2)
   (>= . 2))
  ((<= . 1) (>= . 1))
  (< <= <= > >= >= null? = car cadr apply cdr)
  ((#%lambda #t
     (%%_f724yz0fs__?=)
     ()
     (#%begin
       (#%set! >= (%%_f724yz0fs__?= > >=))
       (#%set! <= (%%_f724yz0fs__?= < <=))))
   (#%lambda #t
     (%%_f7o0w01fs_comparator |%%_f7KYtt1fs_chainer|)
     ()
     (#%lambda #t
       |%%_f74VrW1fs_args|
       (|%%_f7KYtt1fs_chainer| %%_f7o0w01fs_comparator)
       (#%if (null? |%%_f74VrW1fs_args|)
         #t
         (#%if (null? (cdr |%%_f74VrW1fs_args|))
           #t
           (#%if (#%if (= (car |%%_f74VrW1fs_args|)
                          (cadr |%%_f74VrW1fs_args|))
                   #t
                   (%%_f7o0w01fs_comparator
                     (car |%%_f74VrW1fs_args|)
                     (cadr |%%_f74VrW1fs_args|)))
             (apply |%%_f7KYtt1fs_chainer|
                    (cdr |%%_f74VrW1fs_args|))
             #f)))))))
(#%program
  ((gcd . 1)
   (apply . 1)
   (_gcd . 1)
   (car . 2)
   (cdr . 2)
   (|%%_f7MNnQ2fs_args| . 5)
   (null? . 2))
  ()
  (cdr car apply gcd _gcd null?)
  (#%define gcd
    (#%lambda #t
      |%%_f7MNnQ2fs_args|
      ()
      (#%if (null? |%%_f7MNnQ2fs_args|)
        0
        (#%if (null? (cdr |%%_f7MNnQ2fs_args|))
          (car |%%_f7MNnQ2fs_args|)
          (_gcd (car |%%_f7MNnQ2fs_args|)
                (apply gcd (cdr |%%_f7MNnQ2fs_args|))))))))
(#%program
  ((lcm . 1)
   (apply . 1)
   (_lcm . 1)
   (car . 2)
   (cdr . 2)
   (|%%_f76Klh3fs_args| . 5)
   (null? . 2))
  ()
  (cdr car apply lcm _lcm null?)
  (#%define lcm
    (#%lambda #t
      |%%_f76Klh3fs_args|
      ()
      (#%if (null? |%%_f76Klh3fs_args|)
        1
        (#%if (null? (cdr |%%_f76Klh3fs_args|))
          (car |%%_f76Klh3fs_args|)
          (_lcm (car |%%_f76Klh3fs_args|)
                (apply lcm (cdr |%%_f76Klh3fs_args|))))))))
(#%program
  ((|%%_f7sGjK3fs_x| . 1)
   (remainder . 1)
   (+ . 1)
   (|%%_f78zfE4fs_r| . 3)
   (positive? . 1)
   (|%%_f7OChb4fs_y| . 3)
   (negative? . 2))
  ()
  (remainder positive? negative? +)
  (#%define modulo
    (#%lambda #t
      (|%%_f7sGjK3fs_x| |%%_f7OChb4fs_y|)
      ()
      ((#%lambda #t
         (|%%_f78zfE4fs_r|)
         (|%%_f7OChb4fs_y|)
         (#%if ((#%if (negative? |%%_f7OChb4fs_y|)
                  positive?
                  negative?)
                |%%_f78zfE4fs_r|)
           (+ |%%_f78zfE4fs_r| |%%_f7OChb4fs_y|)
           |%%_f78zfE4fs_r|))
       (remainder |%%_f7sGjK3fs_x| |%%_f7OChb4fs_y|)))))
(#%program
  ((- . 1)
   (char->integer . 4)
   (%%_f7wk7s6fs_c . 2)
   (%%_f7ao9_5fs_lc-offset . 1)
   (+ . 1)
   (integer->char . 1)
   (|%%_f7Qrby5fs_z| . 1)
   (<= . 1)
   (%%_f7uvd55fs_a . 2)
   (|%%_f7Sg5V6fs_cv| . 3)
   (>= . 1))
  ()
  (integer->char + >= <= char->integer -)
  (#%define char-downcase
    ((#%lambda #t
       (%%_f7uvd55fs_a)
       ()
       ((#%lambda #t
          (|%%_f7Qrby5fs_z|)
          (%%_f7uvd55fs_a)
          ((#%lambda #t
             (%%_f7ao9_5fs_lc-offset)
             (|%%_f7Qrby5fs_z| %%_f7uvd55fs_a)
             (#%lambda #t
               (%%_f7wk7s6fs_c)
               (%%_f7ao9_5fs_lc-offset
                 |%%_f7Qrby5fs_z|
                 %%_f7uvd55fs_a)
               ((#%lambda #t
                  (|%%_f7Sg5V6fs_cv|)
                  (%%_f7wk7s6fs_c
                    %%_f7ao9_5fs_lc-offset
                    |%%_f7Qrby5fs_z|
                    %%_f7uvd55fs_a)
                  (#%if (#%if (>= |%%_f7Sg5V6fs_cv| %%_f7uvd55fs_a)
                          (<= |%%_f7Sg5V6fs_cv| |%%_f7Qrby5fs_z|)
                          #f)
                    (integer->char
                      (+ |%%_f7Sg5V6fs_cv| %%_f7ao9_5fs_lc-offset))
                    %%_f7wk7s6fs_c))
                (char->integer %%_f7wk7s6fs_c))))
           (- (char->integer #\a) %%_f7uvd55fs_a)))
        (char->integer #\Z)))
     (char->integer #\A))))
(#%program
  ((char->integer . 4)
   (|%%_f7e2ZI8fs_c| . 2)
   (|%%_f7U5_f8fs_uc-offset| . 1)
   (- . 2)
   (integer->char . 1)
   (|%%_f7y91P7fs_z| . 1)
   (<= . 1)
   (%%_f7cd3m7fs_a . 2)
   (|%%_f7A-W99fs_cv| . 3)
   (>= . 1))
  ()
  (integer->char - >= <= char->integer)
  (#%define char-upcase
    ((#%lambda #t
       (%%_f7cd3m7fs_a)
       ()
       ((#%lambda #t
          (|%%_f7y91P7fs_z|)
          (%%_f7cd3m7fs_a)
          ((#%lambda #t
             (|%%_f7U5_f8fs_uc-offset|)
             (|%%_f7y91P7fs_z| %%_f7cd3m7fs_a)
             (#%lambda #t
               (|%%_f7e2ZI8fs_c|)
               (|%%_f7U5_f8fs_uc-offset|
                 |%%_f7y91P7fs_z|
                 %%_f7cd3m7fs_a)
               ((#%lambda #t
                  (|%%_f7A-W99fs_cv|)
                  (|%%_f7e2ZI8fs_c|
                    |%%_f7U5_f8fs_uc-offset|
                    |%%_f7y91P7fs_z|
                    %%_f7cd3m7fs_a)
                  (#%if (#%if (>= |%%_f7A-W99fs_cv| %%_f7cd3m7fs_a)
                          (<= |%%_f7A-W99fs_cv| |%%_f7y91P7fs_z|)
                          #f)
                    (integer->char
                      (- |%%_f7A-W99fs_cv| |%%_f7U5_f8fs_uc-offset|))
                    |%%_f7e2ZI8fs_c|))
                (char->integer |%%_f7e2ZI8fs_c|))))
           (- %%_f7cd3m7fs_a (char->integer #\A))))
        (char->integer #\z)))
     (char->integer #\a))))
(#%program
  ((|%%_f7WWUC9fs_args| . 1)
   (map . 1)
   (|%%_f7gTS3afs_c2| . 1)
   (|%%_f7CPQwafs_c1| . 1)
   (char->integer . 3)
   (> . 1)
   (apply . 1))
  ()
  (map char->integer > apply)
  (#%define char>?
    (#%lambda #t
      (|%%_f7CPQwafs_c1|
        |%%_f7gTS3afs_c2|
        .
        |%%_f7WWUC9fs_args|)
      ()
      (apply >
             (char->integer |%%_f7CPQwafs_c1|)
             (char->integer |%%_f7gTS3afs_c2|)
             (map char->integer |%%_f7WWUC9fs_args|)))))
(#%program
  ((|%%_f7YLOZafs_args| . 1)
   (map . 1)
   (|%%_f7iIMqbfs_c2| . 1)
   (|%%_f7EEKTbfs_c1| . 1)
   (char->integer . 3)
   (< . 1)
   (apply . 1))
  ()
  (map char->integer < apply)
  (#%define char<?
    (#%lambda #t
      (|%%_f7EEKTbfs_c1|
        |%%_f7iIMqbfs_c2|
        .
        |%%_f7YLOZafs_args|)
      ()
      (apply <
             (char->integer |%%_f7EEKTbfs_c1|)
             (char->integer |%%_f7iIMqbfs_c2|)
             (map char->integer |%%_f7YLOZafs_args|)))))
(#%program
  ((char=? . 1)
   (|%%_f7kxGNcfs_c2| . 2)
   (|%%_f7-AIkcfs_c1| . 2)
   (char>? . 1))
  ()
  (char=? char>?)
  (#%define char>=?
    (#%lambda #t
      (|%%_f7-AIkcfs_c1| |%%_f7kxGNcfs_c2|)
      ()
      (#%if (char>? |%%_f7-AIkcfs_c1| |%%_f7kxGNcfs_c2|)
        #t
        (char=? |%%_f7-AIkcfs_c1| |%%_f7kxGNcfs_c2|)))))
(#%program
  ((char=? . 1)
   (|%%_f70qCHdfs_c2| . 2)
   (|%%_f7GtEedfs_c1| . 2)
   (char<? . 1))
  ()
  (char=? char<?)
  (#%define char<=?
    (#%lambda #t
      (|%%_f7GtEedfs_c1| |%%_f70qCHdfs_c2|)
      ()
      (#%if (char<? |%%_f7GtEedfs_c1| |%%_f70qCHdfs_c2|)
        #t
        (char=? |%%_f7GtEedfs_c1| |%%_f70qCHdfs_c2|)))))
(#%program
  ((|%%_f7mmA8efs_args| . 1)
   (map . 1)
   (|%%_f7IiyBefs_c2| . 1)
   (%%_f72fw2ffs_c1 . 1)
   (char-downcase . 3)
   (char>? . 1)
   (apply . 1))
  ()
  (map char-downcase char>? apply)
  (#%define char-ci>?
    (#%lambda #t
      (%%_f72fw2ffs_c1
        |%%_f7IiyBefs_c2|
        .
        |%%_f7mmA8efs_args|)
      ()
      (apply char>?
             (char-downcase %%_f72fw2ffs_c1)
             (char-downcase |%%_f7IiyBefs_c2|)
             (map char-downcase |%%_f7mmA8efs_args|)))))
(#%program
  ((%%_f7obuvffs_args . 1)
   (map . 1)
   (|%%_f7K7sYffs_c2| . 1)
   (%%_f744qpgfs_c1 . 1)
   (char-downcase . 3)
   (char<? . 1)
   (apply . 1))
  ()
  (map char-downcase char<? apply)
  (#%define char-ci<?
    (#%lambda #t
      (%%_f744qpgfs_c1
        |%%_f7K7sYffs_c2|
        .
        %%_f7obuvffs_args)
      ()
      (apply char<?
             (char-downcase %%_f744qpgfs_c1)
             (char-downcase |%%_f7K7sYffs_c2|)
             (map char-downcase %%_f7obuvffs_args)))))
(#%program
  ((|%%_f7q0oSgfs_args| . 1)
   (map . 1)
   (|%%_f7MYljhfs_c2| . 1)
   (|%%_f76VjMhfs_c1| . 1)
   (char-downcase . 3)
   (char=? . 1)
   (apply . 1))
  ()
  (map char-downcase char=? apply)
  (#%define char-ci=?
    (#%lambda #t
      (|%%_f76VjMhfs_c1|
        |%%_f7MYljhfs_c2|
        .
        |%%_f7q0oSgfs_args|)
      ()
      (apply char=?
             (char-downcase |%%_f76VjMhfs_c1|)
             (char-downcase |%%_f7MYljhfs_c2|)
             (map char-downcase |%%_f7q0oSgfs_args|)))))
(#%program
  ((char-ci=? . 1)
   (|%%_f7ONfGifs_c2| . 2)
   (|%%_f7sRhdifs_c1| . 2)
   (char-ci>? . 1))
  ()
  (char-ci=? char-ci>?)
  (#%define char-ci>=?
    (#%lambda #t
      (|%%_f7sRhdifs_c1| |%%_f7ONfGifs_c2|)
      ()
      (#%if (char-ci>? |%%_f7sRhdifs_c1| |%%_f7ONfGifs_c2|)
        #t
        (char-ci=? |%%_f7sRhdifs_c1| |%%_f7ONfGifs_c2|)))))
(#%program
  ((char-ci=? . 1)
   (|%%_f7uGbAjfs_c2| . 2)
   (|%%_f78Kd7jfs_c1| . 2)
   (char-ci<? . 1))
  ()
  (char-ci=? char-ci<?)
  (#%define char-ci<=?
    (#%lambda #t
      (|%%_f78Kd7jfs_c1| |%%_f7uGbAjfs_c2|)
      ()
      (#%if (char-ci<? |%%_f78Kd7jfs_c1| |%%_f7uGbAjfs_c2|)
        #t
        (char-ci=? |%%_f78Kd7jfs_c1| |%%_f7uGbAjfs_c2|)))))
(#%program
  ((|%%_f7QC91kfs_c| . 2) (char<? . 2))
  ()
  (char<?)
  (#%define char-alphabetic?
    (#%lambda #t
      (|%%_f7QC91kfs_c|)
      ()
      (#%if (char<? #\@ |%%_f7QC91kfs_c| #\[)
        #t
        (char<? #\` |%%_f7QC91kfs_c| #\{)))))
(#%program
  ((%%_f7az7ukfs_c . 1) (char<? . 1))
  ()
  (char<?)
  (#%define char-numeric?
    (#%lambda #t
      (%%_f7az7ukfs_c)
      ()
      (char<? #\/ %%_f7az7ukfs_c #\:))))
(#%program
  ((|%%_f7wv5Xkfs_c| . 4) (char=? . 4))
  ()
  (char=?)
  (#%define char-whitespace?
    (#%lambda #t
      (|%%_f7wv5Xkfs_c|)
      ()
      (#%if (char=? |%%_f7wv5Xkfs_c| #\space)
        #t
        (#%if (char=? |%%_f7wv5Xkfs_c| #\tab)
          #t
          (#%if (char=? |%%_f7wv5Xkfs_c| #\newline)
            #t
            (char=? |%%_f7wv5Xkfs_c| #\return)))))))
(#%program
  ((|%%_f7Sr3olfs_c| . 1) (char<? . 1))
  ()
  (char<?)
  (#%define char-upper-case?
    (#%lambda #t
      (|%%_f7Sr3olfs_c|)
      ()
      (char<? #\@ |%%_f7Sr3olfs_c| #\[))))
(#%program
  ((|%%_f7co1Rlfs_c| . 1) (char<? . 1))
  ()
  (char<?)
  (#%define char-lower-case?
    (#%lambda #t
      (|%%_f7co1Rlfs_c|)
      ()
      (char<? #\` |%%_f7co1Rlfs_c| #\{))))
(#%program
  ((void . 1))
  ()
  (void)
  (#%define string-downcase (void)))
(#%program
  ((void . 1))
  ()
  (void)
  (#%define string-upcase (void)))
(#%program
  ((char-upcase . 1)
   (|%%_f7W5T5ofs_newstr| . 1)
   (|%%_f7A9VEnfs_str| . 3)
   (string-upcase . 1)
   (make-string . 2)
   (string-length . 4)
   (char-downcase . 1)
   (|%%_f7edXbnfs_newstr| . 1)
   (|%%_f7UgZKmfs_str| . 3)
   (string-downcase . 1)
   (+ . 1)
   (%%_f7yk_hmfs_string-map . 3)
   (|%%_f7g2Ryofs_strsrc| . 2)
   (string-ref . 1)
   (|%%_f7YWMspfs_proc| . 2)
   (|%%_f7C-O_ofs_strdst| . 3)
   (string-set! . 1)
   (|%%_f7EPImqfs_l| . 2)
   (|%%_f7iTKVpfs_n| . 4)
   (< . 1))
  ((string-upcase . 1)
   (string-downcase . 1)
   (%%_f7yk_hmfs_string-map . 1))
  (string-downcase
    string-length
    char-downcase
    make-string
    string-upcase
    char-upcase
    string-ref
    string-set!
    +
    <)
  (#%letrec #t
    ((%%_f7yk_hmfs_string-map
       (#%lambda #t
         (|%%_f7g2Ryofs_strsrc|
           |%%_f7C-O_ofs_strdst|
           |%%_f7YWMspfs_proc|
           |%%_f7iTKVpfs_n|
           |%%_f7EPImqfs_l|)
         (%%_f7yk_hmfs_string-map)
         (#%if (< |%%_f7iTKVpfs_n| |%%_f7EPImqfs_l|)
           (#%begin
             (string-set!
               |%%_f7C-O_ofs_strdst|
               |%%_f7iTKVpfs_n|
               (|%%_f7YWMspfs_proc|
                 (string-ref
                   |%%_f7g2Ryofs_strsrc|
                   |%%_f7iTKVpfs_n|)))
             (%%_f7yk_hmfs_string-map
               |%%_f7g2Ryofs_strsrc|
               |%%_f7C-O_ofs_strdst|
               |%%_f7YWMspfs_proc|
               (+ |%%_f7iTKVpfs_n| 1)
               |%%_f7EPImqfs_l|))
           |%%_f7C-O_ofs_strdst|))))
    ()
    (#%begin
      (#%set! string-downcase
        (#%lambda #t
          (|%%_f7UgZKmfs_str|)
          (%%_f7yk_hmfs_string-map)
          ((#%lambda #t
             (|%%_f7edXbnfs_newstr|)
             (|%%_f7UgZKmfs_str| %%_f7yk_hmfs_string-map)
             (%%_f7yk_hmfs_string-map
               |%%_f7UgZKmfs_str|
               |%%_f7edXbnfs_newstr|
               char-downcase
               0
               (string-length |%%_f7UgZKmfs_str|)))
           (make-string (string-length |%%_f7UgZKmfs_str|)))))
      (#%set! string-upcase
        (#%lambda #t
          (|%%_f7A9VEnfs_str|)
          (%%_f7yk_hmfs_string-map)
          ((#%lambda #t
             (|%%_f7W5T5ofs_newstr|)
             (|%%_f7A9VEnfs_str| %%_f7yk_hmfs_string-map)
             (%%_f7yk_hmfs_string-map
               |%%_f7A9VEnfs_str|
               |%%_f7W5T5ofs_newstr|
               char-upcase
               0
               (string-length |%%_f7A9VEnfs_str|)))
           (make-string (string-length |%%_f7A9VEnfs_str|))))))))
(#%program
  ((|%%_f7GECJrfs_s2| . 1)
   (|%%_f7kIEgrfs_s1| . 1)
   (string->list . 2)
   (car . 2)
   (cdr . 2)
   (|%%_f7-LGPqfs_s<?| . 2)
   (char>? . 1)
   (%%_f72quxtfs_c2 . 2)
   (|%%_f7Itw4tfs_c1| . 2)
   (char<? . 1)
   (|%%_f7mxyDsfs_s2| . 4)
   (not . 1)
   (|%%_f70BAasfs_s1| . 3)
   (null? . 3))
  ((|%%_f7-LGPqfs_s<?| . 1))
  (string->list car char<? cdr char>? not null?)
  (#%define string<?
    (#%letrec #t
      ((|%%_f7-LGPqfs_s<?|
         (#%lambda #t
           (|%%_f70BAasfs_s1| |%%_f7mxyDsfs_s2|)
           (|%%_f7-LGPqfs_s<?|)
           (#%if (null? |%%_f70BAasfs_s1|)
             (not (null? |%%_f7mxyDsfs_s2|))
             (#%if (null? |%%_f7mxyDsfs_s2|)
               #f
               ((#%lambda #t
                  (|%%_f7Itw4tfs_c1| %%_f72quxtfs_c2)
                  (|%%_f7mxyDsfs_s2|
                    |%%_f70BAasfs_s1|
                    |%%_f7-LGPqfs_s<?|)
                  (#%if (char<? |%%_f7Itw4tfs_c1| %%_f72quxtfs_c2)
                    #t
                    (#%if (char>? |%%_f7Itw4tfs_c1| %%_f72quxtfs_c2)
                      #f
                      (|%%_f7-LGPqfs_s<?|
                        (cdr |%%_f70BAasfs_s1|)
                        (cdr |%%_f7mxyDsfs_s2|)))))
                (car |%%_f70BAasfs_s1|)
                (car |%%_f7mxyDsfs_s2|)))))))
      ()
      (#%lambda #t
        (|%%_f7kIEgrfs_s1| |%%_f7GECJrfs_s2|)
        (|%%_f7-LGPqfs_s<?|)
        (|%%_f7-LGPqfs_s<?|
          (string->list |%%_f7kIEgrfs_s1|)
          (string->list |%%_f7GECJrfs_s2|))))))
(#%program
  ((|%%_f74foUufs_s2| . 1)
   (|%%_f7Kiqrufs_s1| . 1)
   (string->list . 2)
   (car . 2)
   (cdr . 2)
   (%%_f7oms-tfs_s>? . 2)
   (char<? . 1)
   (|%%_f7s0gIwfs_c2| . 2)
   (%%_f764ifwfs_c1 . 2)
   (char>? . 1)
   (%%_f7qbmlvfs_s1 . 4)
   (not . 1)
   (|%%_f7M7kOvfs_s2| . 3)
   (null? . 3))
  ((%%_f7oms-tfs_s>? . 1))
  (string->list car char>? cdr char<? not null?)
  (#%define string>?
    (#%letrec #t
      ((%%_f7oms-tfs_s>?
         (#%lambda #t
           (%%_f7qbmlvfs_s1 |%%_f7M7kOvfs_s2|)
           (%%_f7oms-tfs_s>?)
           (#%if (null? |%%_f7M7kOvfs_s2|)
             (not (null? %%_f7qbmlvfs_s1))
             (#%if (null? %%_f7qbmlvfs_s1)
               #f
               ((#%lambda #t
                  (%%_f764ifwfs_c1 |%%_f7s0gIwfs_c2|)
                  (|%%_f7M7kOvfs_s2|
                    %%_f7qbmlvfs_s1
                    %%_f7oms-tfs_s>?)
                  (#%if (char>? %%_f764ifwfs_c1 |%%_f7s0gIwfs_c2|)
                    #t
                    (#%if (char<? %%_f764ifwfs_c1 |%%_f7s0gIwfs_c2|)
                      #f
                      (%%_f7oms-tfs_s>?
                        (cdr %%_f7qbmlvfs_s1)
                        (cdr |%%_f7M7kOvfs_s2|)))))
                (car %%_f7qbmlvfs_s1)
                (car |%%_f7M7kOvfs_s2|)))))))
      ()
      (#%lambda #t
        (|%%_f7Kiqrufs_s1| |%%_f74foUufs_s2|)
        (%%_f7oms-tfs_s>?)
        (%%_f7oms-tfs_s>?
          (string->list |%%_f7Kiqrufs_s1|)
          (string->list |%%_f74foUufs_s2|))))))
(#%program
  ((string=? . 1)
   (|%%_f78VbCxfs_s2| . 2)
   (|%%_f7OYd9xfs_s1| . 2)
   (string<? . 1))
  ()
  (string=? string<?)
  (#%define string<=?
    (#%lambda #t
      (|%%_f7OYd9xfs_s1| |%%_f78VbCxfs_s2|)
      ()
      (#%if (string<? |%%_f7OYd9xfs_s1| |%%_f78VbCxfs_s2|)
        #t
        (string=? |%%_f7OYd9xfs_s1| |%%_f78VbCxfs_s2|)))))
(#%program
  ((string=? . 1)
   (|%%_f7QN7wyfs_s2| . 2)
   (|%%_f7uR93yfs_s1| . 2)
   (string>? . 1))
  ()
  (string=? string>?)
  (#%define string>=?
    (#%lambda #t
      (|%%_f7uR93yfs_s1| |%%_f7QN7wyfs_s2|)
      ()
      (#%if (string>? |%%_f7uR93yfs_s1| |%%_f7QN7wyfs_s2|)
        #t
        (string=? |%%_f7uR93yfs_s1| |%%_f7QN7wyfs_s2|)))))
(#%program
  ((|%%_f7wG3qzfs_s2| . 1)
   (|%%_f7aK5Zyfs_s1| . 1)
   (string-downcase . 2)
   (string=? . 1))
  ()
  (string-downcase string=?)
  (#%define string-ci=?
    (#%lambda #t
      (|%%_f7aK5Zyfs_s1| |%%_f7wG3qzfs_s2|)
      ()
      (string=?
        (string-downcase |%%_f7aK5Zyfs_s1|)
        (string-downcase |%%_f7wG3qzfs_s2|)))))
(#%program
  ((|%%_f7cz_jAfs_s2| . 1)
   (|%%_f7SC1Tzfs_s1| . 1)
   (string-downcase . 2)
   (string<? . 1))
  ()
  (string-downcase string<?)
  (#%define string-ci<?
    (#%lambda #t
      (|%%_f7SC1Tzfs_s1| |%%_f7cz_jAfs_s2|)
      ()
      (string<?
        (string-downcase |%%_f7SC1Tzfs_s1|)
        (string-downcase |%%_f7cz_jAfs_s2|)))))
(#%program
  ((|%%_f7UrXdBfs_s2| . 1)
   (|%%_f7yvZMAfs_s1| . 1)
   (string-downcase . 2)
   (string>? . 1))
  ()
  (string-downcase string>?)
  (#%define string-ci>?
    (#%lambda #t
      (|%%_f7yvZMAfs_s1| |%%_f7UrXdBfs_s2|)
      ()
      (string>?
        (string-downcase |%%_f7yvZMAfs_s1|)
        (string-downcase |%%_f7UrXdBfs_s2|)))))
(#%program
  ((|%%_f7AkT7Cfs_s2| . 1)
   (|%%_f7eoVGBfs_s1| . 1)
   (string-downcase . 2)
   (string>=? . 1))
  ()
  (string-downcase string>=?)
  (#%define string-ci>=?
    (#%lambda #t
      (|%%_f7eoVGBfs_s1| |%%_f7AkT7Cfs_s2|)
      ()
      (string>=?
        (string-downcase |%%_f7eoVGBfs_s1|)
        (string-downcase |%%_f7AkT7Cfs_s2|)))))
(#%program
  ((|%%_f7gdP1Dfs_s2| . 1)
   (|%%_f7WgRACfs_s1| . 1)
   (string-downcase . 2)
   (string<=? . 1))
  ()
  (string-downcase string<=?)
  (#%define string-ci<=?
    (#%lambda #t
      (|%%_f7WgRACfs_s1| |%%_f7gdP1Dfs_s2|)
      ()
      (string<=?
        (string-downcase |%%_f7WgRACfs_s1|)
        (string-downcase |%%_f7gdP1Dfs_s2|)))))
(#%program
  ((- . 1)
   (make-string . 1)
   (|%%_f7E-GREfs_end| . 2)
   (|%%_f7i2JoEfs_start| . 2)
   (|%%_f7-WEiFfs_newstr| . 2)
   (|%%_f7Y5LXDfs_str| . 1)
   (+ . 2)
   (|%%_f7C9NuDfs_fill-string| . 2)
   (|%%_f7kTCLFfs_sstr| . 2)
   (string-ref . 1)
   (|%%_f70MyFGfs_n| . 2)
   (|%%_f7GPAcGfs_dstr| . 2)
   (string-set! . 1)
   (|%%_f7IEuzHfs_e| . 2)
   (|%%_f7mIw6Hfs_s| . 3)
   (< . 1))
  ((|%%_f7C9NuDfs_fill-string| . 1))
  (make-string - string-ref string-set! + <)
  (#%define substring
    (#%letrec #t
      ((|%%_f7C9NuDfs_fill-string|
         (#%lambda #t
           (|%%_f7kTCLFfs_sstr|
             |%%_f7GPAcGfs_dstr|
             |%%_f70MyFGfs_n|
             |%%_f7mIw6Hfs_s|
             |%%_f7IEuzHfs_e|)
           (|%%_f7C9NuDfs_fill-string|)
           (#%if (< |%%_f7mIw6Hfs_s| |%%_f7IEuzHfs_e|)
             (#%begin
               (string-set!
                 |%%_f7GPAcGfs_dstr|
                 |%%_f70MyFGfs_n|
                 (string-ref |%%_f7kTCLFfs_sstr| |%%_f7mIw6Hfs_s|))
               (|%%_f7C9NuDfs_fill-string|
                 |%%_f7kTCLFfs_sstr|
                 |%%_f7GPAcGfs_dstr|
                 (+ |%%_f70MyFGfs_n| 1)
                 (+ |%%_f7mIw6Hfs_s| 1)
                 |%%_f7IEuzHfs_e|))
             #!void))))
      ()
      (#%lambda #t
        (|%%_f7Y5LXDfs_str|
          |%%_f7i2JoEfs_start|
          |%%_f7E-GREfs_end|)
        (|%%_f7C9NuDfs_fill-string|)
        ((#%lambda #t
           (|%%_f7-WEiFfs_newstr|)
           (|%%_f7E-GREfs_end|
             |%%_f7i2JoEfs_start|
             |%%_f7Y5LXDfs_str|
             |%%_f7C9NuDfs_fill-string|)
           (#%begin
             (|%%_f7C9NuDfs_fill-string|
               |%%_f7Y5LXDfs_str|
               |%%_f7-WEiFfs_newstr|
               0
               |%%_f7i2JoEfs_start|
               |%%_f7E-GREfs_end|)
             |%%_f7-WEiFfs_newstr|))
         (make-string
           (- |%%_f7E-GREfs_end| |%%_f7i2JoEfs_start|)))))))
(#%program
  ((- . 1)
   (cdr . 1)
   (list-ref . 1)
   (|%%_f72Bs0Ifs_list| . 2)
   (car . 1)
   (|%%_f7oxqtIfs_n| . 2)
   (zero? . 1))
  ()
  (list-ref cdr - car zero?)
  (#%define list-ref
    (#%lambda #t
      (|%%_f72Bs0Ifs_list| |%%_f7oxqtIfs_n|)
      ()
      (#%if (zero? |%%_f7oxqtIfs_n|)
        (car |%%_f72Bs0Ifs_list|)
        (list-ref
          (cdr |%%_f72Bs0Ifs_list|)
          (- |%%_f7oxqtIfs_n| 1))))))
(#%program
  ((|%%_f7KtoWIfs_args| . 1)
   (|%%_f74qmnJfs_k| . 1)
   (apply . 1)
   (call-with-current-continuation . 1))
  ()
  (apply call-with-current-continuation)
  (#%define values
    (#%lambda #t
      |%%_f7KtoWIfs_args|
      ()
      (call-with-current-continuation
        (#%lambda #t
          (|%%_f74qmnJfs_k|)
          (|%%_f7KtoWIfs_args|)
          (apply |%%_f74qmnJfs_k| |%%_f7KtoWIfs_args|))))))
