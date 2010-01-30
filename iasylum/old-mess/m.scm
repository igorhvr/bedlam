; (load "m.scm")
(load "mosel-code-generator-utils.scm")

(define model-name "UpToThreeTasksScheduling")
(define input-file-name "input-file.dat")

(define problem-solver-spec
  '(
    (number "NTAREFAS")
    (number "NMAQUINAS")
    (number "NDEPENDENCIAS")
    (set "TAREFAS")
    (alist "PROCESSAMENTO" "array (TAREFAS) of real")
    (alist "PESO" "array (TAREFAS) of real")
    (alist "DEPENDENCIAS" "array (TAREFAS) of string")
    ))

(generate-mosel-code problem-solver-spec)
