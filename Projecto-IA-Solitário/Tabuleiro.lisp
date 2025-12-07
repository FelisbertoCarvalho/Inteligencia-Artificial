;; PROJECTO ;;; puzzle.lisp
;;; Projeto Solitário - Inteligência Artificial 2025/2026
;;; Autor: Felisberto de Carvalho, Tiago Gomes, Filipe Patricio
;;; Descrição: Representação do tabuleiro e operadores do Solitário.

;; valor no código: 1 - significado: há um pino
;; valor no código: 0 - significado: casa vazia
;; valor no código: nil - significado: fora do tabuleiro


;;; Representação do tabuleiro inicial
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


;;; Retorna o n-ésimo elemento de uma lista (recursivamente)
(defun get-nth (n lst)
  (if (zerop n)
      (car lst)
      (get-nth (1- n) (cdr lst))))

;;; Substitui o elemento na posição n de uma lista (recursivamente)
(defun set-nth (n new-val lst)
  (if (zerop n)
      (cons new-val (cdr lst))
      (cons (car lst) (set-nth (1- n) new-val (cdr lst)))))

;;; Acede a uma posição (linha, coluna)
(defun get-pos (linha coluna tab)
  (if (or (< linha 1) (> linha 7) (< coluna 1) (> coluna 7))
      nil
      (let ((linha-val (get-nth (1- linha) tab)))
        (if (null linha-val)
            nil
            (get-nth (1- coluna) linha-val)))))


;;; Substitui um valor numa posição (linha, coluna)
(defun set-pos (linha coluna new-val tab)
  (if (or (< linha 1) (> linha 7) (< coluna 1) (> coluna 7))
      tab  ; devolve o tabuleiro inalterado
      (if (= linha 1)
          (cons (set-nth (1- coluna) new-val (car tab)) (cdr tab))
          (cons (car tab) (set-pos (1- linha) coluna new-val (cdr tab))))))


;;; Verifica se uma posição (linha, coluna) é válida no tabuleiro
(defun pos-valida? (linha coluna tab)
  (cond
    ;; fora dos limites (menor que 1 ou maior que 7)
    ((or (< linha 1) (> linha 7) (< coluna 1) (> coluna 7))
     nil)
    ;; posição é nil (fora da cruz)
    ((null (get-pos linha coluna tab))
     nil)
    ;; caso contrário, é válida
    (t t)))


;;(get-pos 4 4 (tabuleiro-inicial))  ; devolve 0 (centro vazio)
;;(get-pos 3 3 (tabuleiro-inicial))  ; devolve 1 (tem pino)
;;(set-pos 3 3 0 (tabuleiro-inicial)) ; cria um novo tabuleiro com (3,3) vazio
;;(get-pos 8 8 (tabuleiro-inicial)) ; => NIL
;;(get-pos 1 1 (tabuleiro-inicial)) ; => NIL (fora da cruz)
;;(get-pos 4 4 (tabuleiro-inicial)) ; => 0 (centro vazio)

(defun cr (linha coluna tab)
  (if (and
        ;; todas as posições envolvidas são válidas
        (pos-valida? linha coluna tab)
        (pos-valida? linha (+ coluna 1) tab)
        (pos-valida? linha (+ coluna 2) tab)
        ;; condições do movimento
        (= (get-pos linha coluna tab) 1)     ; origem tem pino
        (= (get-pos linha (+ coluna 1) tab) 1) ; pino a ser comido
        (= (get-pos linha (+ coluna 2) tab) 0)) ; destino vazio
      ;; então cria novo tabuleiro recursivamente
      (let* ((tab1 (set-pos linha coluna 0 tab))             ; origem vazia
             (tab2 (set-pos linha (+ coluna 1) 0 tab1))      ; remove o pino do meio
             (tab3 (set-pos linha (+ coluna 2) 1 tab2)))     ; destino ganha pino
        tab3)
      ;; caso contrário, devolve o mesmo tabuleiro (movimento inválido)
      tab))


;;(setq t1 (tabuleiro-inicial))
;;; mover o pino da posição (4,2) para (4,4)
;;(setq t2 (cr 4 2 t1))
;;(get-pos 4 2 t2)  ; 0 -> origem vazia
;;(get-pos 4 3 t2)  ; 0 -> pino comido
;;(get-pos 4 4 t2)  ; 1 -> destino agora tem pino


(defun cl (linha coluna tab)
  (if (and
        ;; todas as posições envolvidas são válidas
        (pos-valida? linha coluna tab)
        (pos-valida? linha (- coluna 1) tab)
        (pos-valida? linha (- coluna 2) tab)
        ;; condições do movimento
        (= (get-pos linha coluna tab) 1)       ; origem tem pino
        (= (get-pos linha (- coluna 1) tab) 1)  ; pino a ser comido
        (= (get-pos linha (- coluna 2) tab) 0)) ; destino vazio
      ;; movimento válido devolve novo tabuleiro
      (let* ((tab1 (set-pos linha coluna 0 tab))             ; origem vazia
             (tab2 (set-pos linha (- coluna 1) 0 tab1))      ; remove o pino do meio
             (tab3 (set-pos linha (- coluna 2) 1 tab2)))     ; destino ganha pino
        tab3)
      ;; movimento inválido  devolve o mesmo tabuleiro
      tab))

;; (setq t1 (tabuleiro-inicial))
;; (setq t2 (ce 4 6 t1))
;; (get-pos 4 6 t2)  ; 0  origem vazia
;; (get-pos 4 5 t2)  ; 0  pino comido
;; (get-pos 4 4 t2)  ; 1  destino ganhou o pino


(defun ct (linha coluna tab)
  (if (and
        ;; posições válidas
        (pos-valida? linha coluna tab)
        (pos-valida? (- linha 1) coluna tab)
        (pos-valida? (- linha 2) coluna tab)
        ;; condições do movimento
        (= (get-pos linha coluna tab) 1)       ; origem com pino
        (= (get-pos (- linha 1) coluna tab) 1) ; pino a ser comido
        (= (get-pos (- linha 2) coluna tab) 0)) ; destino vazio
      ;; aplica as mudanças recursivamente
      (let* ((tab1 (set-pos linha coluna 0 tab))            ; origem vazia
             (tab2 (set-pos (- linha 1) coluna 0 tab1))     ; remove o pino do meio
             (tab3 (set-pos (- linha 2) coluna 1 tab2)))    ; destino ganha o pino
        tab3)
      ;; movimento inválido
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


(defun h1 (tab)
  (/ 1 (+ (o tab) 1)))

(h1 (tabuleiro-inicial))



(defun estados-iguais? (a b)
  (equal a b))

(defun estado-visitado? (estado visitados)
  (cond
    ((null visitados) nil)
    ((estados-iguais? estado (car visitados)) t)
    (t (estado-visitado? estado (cdr visitados)))))



;;Algoritmo  BFS

(defun bfs-aux (fila visitados)
  (cond
    ;; 1. Se a fila está vazia, não há solução
    ((null fila)
     nil)

    (t
     ;; retirar o primeiro elemento da fila
     (let* ((nodo (car fila))
            (estado (first nodo))
            (caminho (second nodo)))

       ;; 2. Se este estado já foi visitado, ignora e continua
       (if (estado-visitado? estado visitados)
           (bfs-aux (cdr fila) visitados)

         ;; 3. Se o estado atual é objetivo  devolve caminho
         (if (objetivo? estado)
             (reverse caminho)

           ;; 4. Caso contrário, gerar sucessores
           ;; Cada sucessor é um par (mov . novo-estado)
           (let ((sucessores (gera-sucessores estado)))

             ;; converter sucessores em nós válidos
             (let ((novos-nodos
                     (mapcar
                       (lambda (par)
                         (let ((mov (car par))
                               (novo-estado (cdr par)))
                           (list novo-estado (cons mov caminho))))
                       sucessores)))

               ;; 5. Chamada recursiva ao BFS:
               ;; - adiciona novos nós ao fim da fila
               ;; - marca estado como visitado
               (bfs-aux
                 (append (cdr fila) novos-nodos)
                 (cons estado visitados))))))))))


(defun bfs (inicial)
  (bfs-aux (list (list inicial '())) '()))


;;;Algoritmo  DFS COM LIMITE DE PROFUNDIDADE

(defun dfs (estado limite)
  (dfs-aux estado '() 0 limite '()))

(defun dfs-aux (estado caminho prof limite visitados)
  (cond
    ;; objetivo
    ((objetivo? estado)
     (reverse caminho))

    ;; limite atingido
    ((>= prof limite)
     :limit)  ; sinaliza que parou por profundidade, não por falha total

    ;; estado já visitado
    ((estado-visitado? estado visitados)
     :fail)

    (t
     ;; gerar sucessores
     (let ((sucessores (gera-sucessores estado)))
       (dfs-l-list sucessores caminho prof limite (cons estado visitados))))))


(defun dfs-l-list (lista-sucessores caminho prof limite visitados)
  (cond
    ((null lista-sucessores) :fail)
    (t
     (let* ((par (car lista-sucessores))
            (mov (car par))
            (novo-est (cdr par))
            (novo-caminho (cons mov caminho)))

       (let ((resultado
              (dfs-aux novo-est
                                novo-caminho
                                (+ prof 1)
                                limite
                                visitados)))
         (if (or (not (eq resultado :fail))
                 (eq resultado :limit))
             resultado
           (dfs-l-list (cdr lista-sucessores)
                       caminho
                       prof
                       limite
                       visitados)))))))


;;; A*

(defun a* (inicial)
  (a*-aux (list (list inicial '() 0 (h2 inicial))) '()))


(defun a*-aux (abertos fechados)
  ;; se não há mais nós a expandir  falha
  (cond
    ((null abertos) :fail)

    (t
     ;; ordenar a lista aberta pelo menor f
     (let* ((ordenados
             (sort (copy-list abertos)
                   (lambda (n1 n2)
                     (< (+ (third n1) (fourth n1))  ; g+h
                        (+ (third n2) (fourth n2))))))

            (nodo (car ordenados))
            (estado (first nodo))
            (caminho (second nodo))
            (g (third nodo)))

       ;; se estado já explorado, ignora
       (if (estado-visitado? estado fechados)
           (a*-aux (cdr ordenados) fechados)

         ;; se objetivo  devolve caminho
         (if (objetivo? estado)
             (reverse caminho)

           ;; gerar sucessores
           (let* ((sucessores (gera-sucessores estado))
                  (novos-nos
                    (mapcar
                      (lambda (par)
                        (let ((mov (car par))
                              (novo-est (cdr par)))
                          (list novo-est
                                (cons mov caminho)
                                (+ g 1)
                                (h2 novo-est))))
                      sucessores)))

             ;; recursão com:
             ;; - adicionar sucessores aos abertos
             ;; - estado atual passa para fechados
             (a*-aux
               (append (cdr ordenados) novos-nos)
               (cons estado fechados)))))))))


(defun h-pinos (tab)
  (let ((p (conta-pinos tab)))
    (max 0 (- p 1))))


(defun pinos-bloqueados (tab)
  (labels ((conta-na-pos (linha coluna)
             (cond
               ((or (< linha 1) (> linha 7) (< coluna 1) (> coluna 7)) 0)
               ((null (get-pos linha coluna tab)) 0)
               ((= (get-pos linha coluna tab) 0) 0)
               ((null (movimentos-validos-de-pos linha coluna tab)) 1)
               (t 0))))

    (labels ((aux-linhas (l)
               (if (> l 7) 0
                 (+ (aux-colunas l 1)
                    (aux-linhas (+ l 1)))))

             (aux-colunas (l c)
               (if (> c 7) 0
                 (+ (conta-na-pos l c)
                    (aux-colunas l (+ c 1))))))

      (aux-linhas 1))))

(defun h2 (tab)
  (+ (h-pinos tab)
     (pinos-bloqueados tab)))



;; Algoritmo IDA* 

(defun ida* (inicial)
  (let ((limite (+ 0 (h2 inicial))))
    (ida*-iter inicial limite)))


;;Iterador do IDA*
(defun ida*-iter (estado limite)
  (let ((resultado (ida*-dfs estado '() 0 limite '() most-positive-fixnum)))
    (cond
      ((consp resultado) resultado)  ; caminho encontrado
      ((= resultado most-positive-fixnum) :fail) ; impossível
      (t (ida*-iter estado resultado))))) ; aumentar limite e repetir


;;DFS do IDA*
(defun ida*-dfs (estado caminho g limite visitados melhor-ultrapassou)
  (let* ((h (h2 estado))
         (f (+ g h)))  ; custo total
    (cond
      ;; 1. Se f > limite  este estado não pode ser expandido
      ((> f limite)
       (min melhor-ultrapassou f))

      ;; 2. Objetivo encontrado
      ((objetivo? estado)
       (reverse caminho))

      ;; 3. Estado repetido  evita ciclos
      ((estado-visitado? estado visitados)
       melhor-ultrapassou)

      (t
       ;; 4. Explora sucessores
       (let ((sucessores (gera-sucessores estado)))
         (ida*-dfs-expand sucessores caminho g limite
                          (cons estado visitados)
                          melhor-ultrapassou))))))



;;Expansão dos sucessores, um por um
(defun ida*-dfs-expand (sucessores caminho g limite visitados melhor-ultrapassou)
  (cond
    ;; sem sucessores: devolve o melhor limite ultrapassado até agora
    ((null sucessores) melhor-ultrapassou)

    (t
     (let* ((par (car sucessores))
            (mov (car par))
            (novo-est (cdr par))
            (resultado
              (ida*-dfs novo-est
                        (cons mov caminho)
                        (+ g 1)
                        limite
                        visitados
                        melhor-ultrapassou)))

       ;; se encontrou caminho devolve caminho
       (if (consp resultado)
           resultado

         ;; caso contrário, atualiza o melhor ultrapassado e continua
         (ida*-dfs-expand (cdr sucessores)
                          caminho
                          g
                          limite
                          visitados
                          (min melhor-ultrapassou resultado)))))))


;; (setq t0 (tabuleiro-inicial))
;;(ida* t0)
 
;; ALGORITMO SMA*

(defun make-no (estado caminho g h fmin)
  (list estado caminho g h fmin))

(defun no-estado  (no) (nth 0 no))
(defun no-caminho (no) (nth 1 no))
(defun no-g       (no) (nth 2 no))
(defun no-h       (no) (nth 3 no))
(defun no-fmin    (no) (nth 4 no))


(defun ordenar-abertos (abertos)
  (sort (copy-list abertos)
        (lambda (n1 n2)
          (< (+ (no-g n1) (no-h n1))
             (+ (no-g n2) (no-h n2))))))


(defun expandir-no (no)
  (let* ((estado (no-estado no))
         (caminho (no-caminho no))
         (g (no-g no))
         (pares (gera-sucessores estado)))
    (mapcar
     (lambda (p)
       (let* ((mov (car p))
              (novo (cdr p))
              (ng (+ g 1))
              (nh (h2 novo)))
         (make-no novo (cons mov caminho) ng nh most-positive-fixnum)))
     pares)))


(defun pior-no (abertos pior)
  (cond
    ((null abertos) pior)
    (t (let* ((n (car abertos))
              (fn (+ (no-g n) (no-h n)))
              (fp (if pior (+ (no-g pior) (no-h pior)) -1))
              (novo-pior (if (> fn fp) n pior)))
         (pior-no (cdr abertos) novo-pior)))))

(defun remover-pior (abertos)
  (let ((p (pior-no abertos nil)))
    (remove p abertos :test #'equal)))

(defun cortar-ate (abertos max)
  (if (<= (length abertos) max)
      abertos
      (cortar-ate (remover-pior abertos) max)))


(defun sma*-aux (abertos max-nos)
  (cond
    ((null abertos) :fail)

    (t
     (let* ((ordenados (ordenar-abertos abertos))
            (n0 (car ordenados))
            (resto (cdr ordenados)))

       ;; caso objetivo
       (if (objetivo? (no-estado n0))
           (reverse (no-caminho n0))

         ;; expandir nó
         (let* ((filhos (expandir-no n0))
                (novos (append filhos resto))
                (limitados (cortar-ate novos max-nos)))

           ;; recursão
           (sma*-aux limitados max-nos)))))))

(defun sma* (inicial max-nos)
  (let* ((h0 (h2 inicial))
         (root (make-no inicial '() 0 h0 h0)))
    (sma*-aux (list root) max-nos)))
