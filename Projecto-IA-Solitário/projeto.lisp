;;; projeto.lisp
;;; Interface principal com o utilizador
;;; Carrega: puzzle.lisp e procura.lisp
;;; Lê: problemas.dat


;; Fazer (setf *default-pathname-defaults* #P"C:/PastaProjeto/") no Listener

(defun carregar-modulos ()
  "Carrega os ficheiros puzzle.lisp e procura.lisp"
  (load "puzzle.lisp")
  ; (load "procura.lisp")
)


(defun ler-problemas ()
  "Lê os problemas do ficheiro problemas.dat"
    (with-open-file (stream "problemas.dat")
      (loop for prob = (read stream nil)
            while prob
            collect prob))
)

(defun print-problemas ()
  "Imprime a lista dos tabuleiros disponíveis"
  (format t "Tabuleiros disponíveis: ~%")
  (loop for i from 1 to (length (ler-problemas)) do
        (format t "~A -> Tabuleiro ~A~%" i i))
)

(defun escolher-problema ()
  "Permite ao utilizador escolher um problema pelo índice e devolve o tabuleiro escolhido"
  (print-problemas)
  (format t "Escolha um problema: ")
  (let ((op (read)))
    (nth (1- op) (ler-problemas)))
)


