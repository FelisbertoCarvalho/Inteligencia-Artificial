;;; Projeto Solitario - Inteligencia Artificial 2025/2026
;;; Autores: 
;; Felisberto de Carvalho 202200359
;; Tiago Campos 202300064
;; Filipe Patricio 202300133

;;; puzzle.lisp
;;; Representacao do tabuleiro e operadores de movimento do Solitario.
;; valor no codigo: 1 - significado: ha um pino
;; valor no codigo: 0 - significado: casa vazia
;; valor no codigo: nil - significado: fora do tabuleiro


;;; Representacao do tabuleiro inicial
(defun tabuleiro-inicial ()
  '((nil nil 1 1 1 nil nil)
    (nil nil 1 1 1 nil nil)
    (1 1 1 1 1 1 1)
    (1 1 1 0 1 1 1)
    (1 1 1 1 1 1 1)
    (nil nil 1 1 1 nil nil)
    (nil nil 1 1 1 nil nil)))


(defun tabuleiro-A ()
  '((nil nil 0 0 0 nil nil)
    (nil nil 0 0 0 nil nil)
    (0 0 0 0 0 0 0)
    (0 0 0 0 0 0 0)
    (0 0 0 0 1 1 0)
    (nil nil 0 1 0 nil nil)
    (nil nil 0 0 0 nil nil)))


(defun tabuleiro-B ()
  '((nil nil 0 0 0 nil nil)
    (nil nil 0 0 0 nil nil)
    (0 0 0 0 0 0 0)
    (0 0 0 1 0 0 0)
    (0 0 0 1 1 1 0)
    (nil nil 0 0 0 nil nil)
    (nil nil 0 0 0 nil nil)))


(defun tabuleiro-C ()
  '((nil nil 0 0 0 nil nil)
    (nil nil 0 0 0 nil nil)
    (0 0 0 0 1 0 0)
    (0 0 0 0 1 1 0)
    (0 0 0 1 1 1 0)
    (nil nil 0 0 0 nil nil)
    (nil nil 0 0 0 nil nil)))


(defun tabuleiro-D ()
  '((nil nil 0 0 0 nil nil)
    (nil nil 0 0 1 nil nil)
    (0 0 0 0 1 1 0)
    (0 0 0 0 1 1 1)
    (0 0 0 1 1 1 1)
    (nil nil 0 0 0 nil nil)
    (nil nil 0 0 0 nil nil)))


(defun tabuleiro-E ()
  '((nil nil 0 0 0 nil nil)
    (nil nil 0 0 0 nil nil)
    (0 0 0 1 1 1 1)
    (0 0 0 0 1 1 1)
    (0 0 0 1 1 1 1)
    (nil nil 0 0 0 nil nil)
    (nil nil 0 0 0 nil nil)))


;;; Retorna o n-esimo elemento de uma lista (recursivamente)
(defun get-nth (n lst)
  (if (zerop n)
      (car lst)
      (get-nth (1- n) (cdr lst))))

;;; Substitui o elemento na posicao n de uma lista (recursivamente)
(defun set-nth (n new-val lst)
  (if (zerop n)
      (cons new-val (cdr lst))
      (cons (car lst) (set-nth (1- n) new-val (cdr lst)))))

;;; Acede a uma posicao (linha, coluna)
(defun get-pos (linha coluna tab)
  (if (or (< linha 1) (> linha 7) (< coluna 1) (> coluna 7))
      nil
      (let ((linha-val (get-nth (1- linha) tab)))
        (if (null linha-val)
            nil
            (get-nth (1- coluna) linha-val)))))


;;; Substitui um valor numa posicao (linha, coluna)
(defun set-pos (linha coluna new-val tab)
  (if (or (< linha 1) (> linha 7) (< coluna 1) (> coluna 7))
      tab  ; devolve o tabuleiro inalterado
      (if (= linha 1)
          (cons (set-nth (1- coluna) new-val (car tab)) (cdr tab))
          (cons (car tab) (set-pos (1- linha) coluna new-val (cdr tab))))))


;;; Verifica se uma posicao (linha, coluna) e valida no tabuleiro
(defun pos-valida? (linha coluna tab)
  (cond
    ;; fora dos limites (menor que 1 ou maior que 7)
    ((or (< linha 1) (> linha 7) (< coluna 1) (> coluna 7))
     nil)
    ;; posicao e nil (fora da cruz)
    ((null (get-pos linha coluna tab))
     nil)
    ;; caso contrario, e valida
    (t t)))


;;(get-pos 4 4 (tabuleiro-inicial))  ; devolve 0 (centro vazio)
;;(get-pos 3 3 (tabuleiro-inicial))  ; devolve 1 (tem pino)
;;(set-pos 3 3 0 (tabuleiro-inicial)) ; cria um novo tabuleiro com (3,3) vazio
;;(get-pos 8 8 (tabuleiro-inicial)) ; => NIL
;;(get-pos 1 1 (tabuleiro-inicial)) ; => NIL (fora da cruz)
;;(get-pos 4 4 (tabuleiro-inicial)) ; => 0 (centro vazio)

(defun cr (linha coluna tab)
  (if (and
        ;; todas as posicoes envolvidas sao validas
        (pos-valida? linha coluna tab)
        (pos-valida? linha (+ coluna 1) tab)
        (pos-valida? linha (+ coluna 2) tab)
        ;; condicoes do movimento
        (= (get-pos linha coluna tab) 1)     ; origem tem pino
        (= (get-pos linha (+ coluna 1) tab) 1) ; pino a ser comido
        (= (get-pos linha (+ coluna 2) tab) 0)) ; destino vazio
      ;; entao cria novo tabuleiro recursivamente
      (let* ((tab1 (set-pos linha coluna 0 tab))             ; origem vazia
             (tab2 (set-pos linha (+ coluna 1) 0 tab1))      ; remove o pino do meio
             (tab3 (set-pos linha (+ coluna 2) 1 tab2)))     ; destino ganha pino
        tab3)
      ;; caso contrario, devolve o mesmo tabuleiro (movimento invalido)
      tab))


;;(setq t1 (tabuleiro-inicial))
;;; mover o pino da posicao (4,2) para (4,4)
;;(setq t2 (cr 4 2 t1))
;;(get-pos 4 2 t2)  ; 0 -> origem vazia
;;(get-pos 4 3 t2)  ; 0 -> pino comido
;;(get-pos 4 4 t2)  ; 1 -> destino agora tem pino


(defun cl (linha coluna tab)
  (if (and
        ;; todas as posicoes envolvidas sao validas
        (pos-valida? linha coluna tab)
        (pos-valida? linha (- coluna 1) tab)
        (pos-valida? linha (- coluna 2) tab)
        ;; condioes do movimento
        (= (get-pos linha coluna tab) 1)       ; origem tem pino
        (= (get-pos linha (- coluna 1) tab) 1)  ; pino a ser comido
        (= (get-pos linha (- coluna 2) tab) 0)) ; destino vazio
      ;; movimento valido devolve novo tabuleiro
      (let* ((tab1 (set-pos linha coluna 0 tab))             ; origem vazia
             (tab2 (set-pos linha (- coluna 1) 0 tab1))      ; remove o pino do meio
             (tab3 (set-pos linha (- coluna 2) 1 tab2)))     ; destino ganha pino
        tab3)
      ;; movimento invalido  devolve o mesmo tabuleiro
      tab))

;; (setq t1 (tabuleiro-inicial))
;; (setq t2 (ce 4 6 t1))
;; (get-pos 4 6 t2)  ; 0  origem vazia
;; (get-pos 4 5 t2)  ; 0  pino comido
;; (get-pos 4 4 t2)  ; 1  destino ganhou o pino


(defun ct (linha coluna tab)
  (if (and
        ;; posicoes validas
        (pos-valida? linha coluna tab)
        (pos-valida? (- linha 1) coluna tab)
        (pos-valida? (- linha 2) coluna tab)
        ;; condicoes do movimento
        (= (get-pos linha coluna tab) 1)       ; origem com pino
        (= (get-pos (- linha 1) coluna tab) 1) ; pino a ser comido
        (= (get-pos (- linha 2) coluna tab) 0)) ; destino vazio
      ;; aplica as mudancas recursivamente
      (let* ((tab1 (set-pos linha coluna 0 tab))            ; origem vazia
             (tab2 (set-pos (- linha 1) coluna 0 tab1))     ; remove o pino do meio
             (tab3 (set-pos (- linha 2) coluna 1 tab2)))    ; destino ganha o pino
        tab3)
      ;; movimento invalido
      tab))


;;(get-pos 6 4 (tabuleiro-inicial)) ; 1  pino que vai mover
;;(get-pos 5 4 (tabuleiro-inicial)) ; 1  pino a ser comido
;;(get-pos 4 4 (tabuleiro-inicial)) ; 0  destino vazio
;;(setq t1 (tabuleiro-inicial))
;;(setq t2 (cc 6 4 t1))
;;(get-pos 6 4 t2) ; 0  origem vazia
;;(get-pos 5 4 t2) ; 0  pino comido
;;(get-pos 4 4 t2) ; 1  destino com pino


(defun cb (linha coluna tab)
  (if (and
        ;; posições válidas
        (pos-valida? linha coluna tab)
        (pos-valida? (+ linha 1) coluna tab)
        (pos-valida? (+ linha 2) coluna tab)
        ;; condições do movimento
        (= (get-pos linha coluna tab) 1)        ; origem com pino
        (= (get-pos (+ linha 1) coluna tab) 1)  ; pino a ser comido
        (= (get-pos (+ linha 2) coluna tab) 0)) ; destino vazio
      ;; aplica as mudanças recursivamente
      (let* ((tab1 (set-pos linha coluna 0 tab))             ; origem vazia
             (tab2 (set-pos (+ linha 1) coluna 0 tab1))      ; remove o pino do meio
             (tab3 (set-pos (+ linha 2) coluna 1 tab2)))     ; destino ganha pino
        tab3)
      ;; movimento inválido  devolve o mesmo tabuleiro
      tab))


;;(setq t1 (tabuleiro-inicial))
;;(setq t2 (cb 2 4 t1))
;;(get-pos 2 4 t2) ; 0  origem vazia
;;(get-pos 3 4 t2) ; 0  pino comido
;;(get-pos 4 4 t2) ; 1  destino com pino


(defun movimentos-validos-de-pos (linha coluna tab)
  (let ((cr-valid (and (pos-valida? linha coluna tab)
                       (pos-valida? linha (+ coluna 1) tab)
                       (pos-valida? linha (+ coluna 2) tab)
                       (= (get-pos linha coluna tab) 1)
                       (= (get-pos linha (+ coluna 1) tab) 1)
                       (= (get-pos linha (+ coluna 2) tab) 0)))
        (cl-valid (and (pos-valida? linha coluna tab)
                       (pos-valida? linha (- coluna 1) tab)
                       (pos-valida? linha (- coluna 2) tab)
                       (= (get-pos linha coluna tab) 1)
                       (= (get-pos linha (- coluna 1) tab) 1)
                       (= (get-pos linha (- coluna 2) tab) 0)))
        (ct-valid (and (pos-valida? linha coluna tab)
                       (pos-valida? (- linha 1) coluna tab)
                       (pos-valida? (- linha 2) coluna tab)
                       (= (get-pos linha coluna tab) 1)
                       (= (get-pos (- linha 1) coluna tab) 1)
                       (= (get-pos (- linha 2) coluna tab) 0)))
        (cb-valid (and (pos-valida? linha coluna tab)
                       (pos-valida? (+ linha 1) coluna tab)
                       (pos-valida? (+ linha 2) coluna tab)
                       (= (get-pos linha coluna tab) 1)
                       (= (get-pos (+ linha 1) coluna tab) 1)
                       (= (get-pos (+ linha 2) coluna tab) 0))))
    (append
      (if cr-valid (list (list 'cr linha coluna)) '())
      (if cl-valid (list (list 'cl linha coluna)) '())
      (if ct-valid (list (list 'ct linha coluna)) '())
      (if cb-valid (list (list 'cb linha coluna)) '()))))


"Aplica mov (por exemplo '(cd 4 2)) ao tabuleiro e devolve o novo tabuleiro se o mov for desconhecido devolve NIL"

(defun aplica-movimento (mov tab)
  (let ((tipo (first mov))
        (linha (second mov))
        (coluna (third mov)))
    (cond
      ((eq tipo 'cr) (cr linha coluna tab))
      ((eq tipo 'cl) (cl linha coluna tab))
      ((eq tipo 'ct) (ct linha coluna tab))
      ((eq tipo 'cb) (cb linha coluna tab))
      (t (error "Tipo de movimento desconhecido: ~A" tipo)))))


"Devolve lista de pares (movimento . novo-tabuleiro) com todos os movimentos válidos do tabuleiro"
(defun gera-sucessores (tab)
  (labels
      ;; gera para colunas de 1..7 recursivamente
      ((iter-cols (linha coluna acc)
         (cond
           ((> coluna 7) acc)
           (t
            (let* ((movs (movimentos-validos-de-pos linha coluna tab))
                   (pares (mapcar (lambda (m) (cons m (aplica-movimento m tab))) movs))
                   (novo-acc (append acc pares)))
              (iter-cols linha (1+ coluna) novo-acc))))))
    ;; gera para linhas de 1..7 recursivamente, reutilizando iter-cols
    (labels ((iter-linhas (linha acc)
               (cond
                 ((> linha 7) acc)
                 (t (iter-linhas (1+ linha) (iter-cols linha 1 acc))))))
      (iter-linhas 1 '()))))


;; (setq t0 (tabuleiro-inicial))
;; (setq suc (gera-sucessores t0))
;; (length suc) ; número de movimentos válidos no estado inicial
;; (first suc)  ; primeiro par (mov . novo-tab)
;; (car (first suc)) ; o movimento (por ex. (cd 4 2))
;; (cdr (first suc)) ; o novo tabuleiro após esse movimento



 "Imprime uma linha do tabuleiro, convertendo 1 , 0 , nil espaço."
(defun print-linha (linha)
  (cond
    ((null linha) (format t "~%")) ; fim da linha
    (t
     (let ((celula (car linha)))
       (cond
         ((null celula) (format t "   "))   ; fora da cruz
         ((= celula 1) (format t " X "))   ; pino
         ((= celula 0) (format t " O "))))
     (print-linha (cdr linha))))) ; recursão para o resto da linha
     

 "Imprime o tabuleiro completo de forma visual."
(defun print-tabuleiro (tab)
  (cond
    ((null tab) (format t "~%")) ; fim das linhas
    (t
     (print-linha (car tab))     ; imprime primeira linha
     (print-tabuleiro (cdr tab))))) ; recursão para as restantes


;; (print-tabuleiro (tabuleiro-inicial))


(defun conta-pinos-linha (linha)
  (cond
    ((null linha) 0)
    ((null (car linha)) (conta-pinos-linha (cdr linha)))
    ((= (car linha) 1) (+ 1 (conta-pinos-linha (cdr linha))))
    (t (conta-pinos-linha (cdr linha)))))


(defun conta-pinos (tab)
  (cond
    ((null tab) 0)
    (t (+ (conta-pinos-linha (car tab))
          (conta-pinos (cdr tab))))))


(defun objetivo? (tab)
  (= (conta-pinos tab) 1))


(defun o (tab)
  (length (gera-sucessores tab)))

(defun estados-iguais? (a b)
  (equal a b))

(defun estado-visitado? (estado visitados)
  (cond
    ((null visitados) nil)
    ((estados-iguais? estado (car visitados)) t)
    (t (estado-visitado? estado (cdr visitados)))))
