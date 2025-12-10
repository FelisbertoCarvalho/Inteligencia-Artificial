;;; Projeto Solitario - Inteligencia Artificial 2025/2026
;;; Autores: 
;; Felisberto de Carvalho 202200359
;; Tiago Campos 202300064
;; Filipe Patricio 202300133

;;; procura.lisp
;;; Implementacao dos algoritmos de procura e respetivas heuristicas
;;; Algoritmos: BFS, DFS, A*, IDA* e SMA*


;;Algoritmo  BFS

(defun bfs-aux (fila visitados)
  (cond
    ;; 1. Se a fila esta¡ vazia, naoo ha¡ solucao
    ((null fila)
     nil)

    (t
     ;; retirar o primeiro elemento da fila
     (let* ((nodo (car fila))
            (estado (first nodo))
            (caminho (second nodo)))

       ;; 2. Se este estado ja¡ foi visitado, ignora e continua
       (if (estado-visitado? estado visitados)
           (bfs-aux (cdr fila) visitados)

         ;; 3. Se o estado atual do objetivo  devolve caminho
         (if (objetivo? estado)
             (reverse caminho)

           ;; 4. Caso contrario, gerar sucessores
           ;; Cada sucessor e um par (mov . novo-estado)
           (let ((sucessores (gera-sucessores estado)))

             ;; converter sucessores em nos validos
             (let ((novos-nodos
                     (mapcar
                       (lambda (par)
                         (let ((mov (car par))
                               (novo-estado (cdr par)))
                           (list novo-estado (cons mov caminho))))
                       sucessores)))

               ;; 5. Chamada recursiva ao BFS:
               ;; - adiciona novos nos ao fim da fila
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
     :limit)  ; sinaliza que parou por profundidade, nao por falha total

    ;; estado ja visitado
    ((estado-visitado? estado visitados)
     :fail)

    (t
     ;; gerar sucessores
     (let ((sucessores (gera-sucessores estado)))
       (dfs-l-list sucessores caminho prof limite (cons estado visitados))))))


(defun dfs-l-list (lista-sucessores caminho prof limite visitados)  ;Percorre recursivamente a lista de sucessores, tentando cada sucessor pela ordem.
  (cond
    ((null lista-sucessores) :fail)           ;; 1) Se não há sucessores restantes, falha (nesta ramificação não encontrou solução).  
    (t                                        ;; 2) Caso haja sucessores, processa o primeiro (car) e prepara dados para explorar
     (let* ((par (car lista-sucessores))
            (mov (car par))
            (novo-est (cdr par))
            (novo-caminho (cons mov caminho)))

       (let ((resultado                       ;; 3) Chama dfs-aux no sucessor atual, aumentando a profundidade (prof + 1)
              (dfs-aux novo-est
                                novo-caminho
                                (+ prof 1)
                                limite
                                visitados)))
         (if (or (not (eq resultado :fail))  ;; 4) Se o resultado NÃO for :fail (ou for :limit), devolve-o
                 (eq resultado :limit))
             resultado
           (dfs-l-list (cdr lista-sucessores)  ;5) Caso contrário (resultado = :fail), tenta o próximo sucessor da lista
                       caminho
                       prof
                       limite
                       visitados)))))))


;;; A*

(defun a* (inicial h)
  (a*-aux (list (list inicial '() 0 h)) '()))


(defun a*-aux (abertos fechados)
  ;; se nao ha mais nos a expandir falha
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

       ;; se estado ja explorado, ignora
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

             ;; recursao com:
             ;; - adicionar sucessores aos abertos
             ;; - estado atual passa para fechados
             (a*-aux
               (append (cdr ordenados) novos-nos)
               (cons estado fechados)))))))))

;; Heuristica 1
(defun h1 (tab)
  (/ 1 (+ (o tab) 1)))

(defun h-pinos (tab)         ;; Heurística auxiliar: conta pinos (com ajuste)
  (let ((p (conta-pinos tab)))
    (max 0 (- p 1))))


(defun pinos-bloqueados (tab)  ;; Conta quantos pinos estão "bloqueados" (não têm movimentos válidos)
  (labels ((conta-na-pos (linha coluna)
             (cond               ;; fora dos limites do tabuleiro 7x7 -> 0
               ((or (< linha 1) (> linha 7) (< coluna 1) (> coluna 7)) 0)
               ((null (get-pos linha coluna tab)) 0)    ;; posição considerada 'nil' na representação (fora da cruz) -> 0
               ((= (get-pos linha coluna tab) 0) 0)    ;; casa vazia -> 0
               ((null (movimentos-validos-de-pos linha coluna tab)) 1)   ;; se há pino mas não existem movimentos válidos a partir daqui -> conta 1
               (t 0))))

    (labels ((aux-linhas (l)  ;; funções auxiliares para percorrer a matriz linha a linha
               (if (> l 7) 0
                 (+ (aux-colunas l 1)
                    (aux-linhas (+ l 1)))))

             (aux-colunas (l c)
               (if (> c 7) 0
                 (+ (conta-na-pos l c)
                    (aux-colunas l (+ c 1))))))

      (aux-linhas 1))))

(defun h2 (tab)                          ;;"Heurística composta utilizada como h(n) nas procuras informadas.
  (+ (h-pinos tab)                       ;;Combina duas componentes:
     (pinos-bloqueados tab)))            ;;- h-pinos(tab): aproximação do número de pinos restantes (p-1)
                                         ;;- pinos-bloqueados(tab): penalização por pinos sem movimentos possíveis


"Retorna a soma das duas componentes. Valores maiores significam estados piores
(do ponto de vista da heurística)"


;; Algoritmo IDA* 

(defun ida* (inicial h)
  (let ((limite (+ 0 h)))
    (ida*-iter inicial limite)))


;;Iterador do IDA*
(defun ida*-iter (estado limite)
  (let ((resultado (ida*-dfs estado '() 0 limite '() most-positive-fixnum)))
    (cond
      ((consp resultado) resultado)  ; caminho encontrado
      ((= resultado most-positive-fixnum) :fail) ; impossÃ­vel
      (t (ida*-iter estado resultado))))) ; aumentar limite e repetir


;;DFS do IDA*
(defun ida*-dfs (estado caminho g limite visitados melhor-ultrapassou)
  (let* ((h (h2 estado))
         (f (+ g h)))  ; custo total
    (cond
      ;; 1. Se f > limite  este estado nÃ£o pode ser expandido
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



;;Expansao dos sucessores, um por um
(defun ida*-dfs-expand (sucessores caminho g limite visitados melhor-ultrapassou)
  (cond
    ;; sem sucessores: devolve o melhor limite ultrapassado ate agora
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

         ;; caso contrario, atualiza o melhor ultrapassado e continua
         (ida*-dfs-expand (cdr sucessores)
                          caminho
                          g
                          limite
                          visitados
                          (min melhor-ultrapassou resultado)))))))


;; (setq t0 (tabuleiro-inicial))
;;(ida* t0)
 
;; ALGORITMO SMA*

;; SMA* é uma versão do A* que funciona com memória limitada.
;; Quando a lista de nós abertos atinge o máximo permitido,
;; remove os piores nós (com maior f = g+h).

;; Cria um nó com todos os seus componentes

(defun make-no (estado caminho g h fmin)
  (list estado caminho g h fmin))

;; Acessores simples para cada campo do nó
(defun no-estado  (no) (nth 0 no))
(defun no-caminho (no) (nth 1 no))
(defun no-g       (no) (nth 2 no))
(defun no-h       (no) (nth 3 no))
(defun no-fmin    (no) (nth 4 no))

;; Ordena os nós abertos pelo menor valor f = g + h
(defun ordenar-abertos (abertos)
  (sort (copy-list abertos)
        (lambda (n1 n2)
          (< (+ (no-g n1) (no-h n1))
             (+ (no-g n2) (no-h n2))))))

;; Expande um nó: gera os seus filhos e cria novos nós com g,h atualizados
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

;; Devolve o pior nó da lista aberta (maior f)
(defun pior-no (abertos pior)
  (cond
    ((null abertos) pior)
    (t (let* ((n (car abertos))
              (fn (+ (no-g n) (no-h n)))
              (fp (if pior (+ (no-g pior) (no-h pior)) -1))
              (novo-pior (if (> fn fp) n pior)))
         (pior-no (cdr abertos) novo-pior)))))

;; Remove o pior nó da lista aberta
(defun remover-pior (abertos)
  (let ((p (pior-no abertos nil)))
    (remove p abertos :test #'equal)))

;; Corta a lista aberta para não ultrapassar o limite max-nos
(defun cortar-ate (abertos max)
  (if (<= (length abertos) max)
      abertos
      (cortar-ate (remover-pior abertos) max)))


;; Função principal auxiliar do SMA*
(defun sma*-aux (abertos max-nos)
  (cond
    ((null abertos) :fail)

    (t ;; Escolhe o nó com menor f
     (let* ((ordenados (ordenar-abertos abertos))
            (n0 (car ordenados))
            (resto (cdr ordenados)))

       ;; caso objetivo
       (if (objetivo? (no-estado n0))
           (reverse (no-caminho n0))

         ;; expandir no
         (let* ((filhos (expandir-no n0))
                (novos (append filhos resto))
                (limitados (cortar-ate novos max-nos)))

           ;; recursao
           (sma*-aux limitados max-nos)))))))

;; Interface: recebe estado inicial e máximo de nós permitidos
(defun sma* (inicial max-nos h)
  (let* ((h0 h)
         (root (make-no inicial '() 0 h0 h0)))
    (sma*-aux (list root) max-nos)))
