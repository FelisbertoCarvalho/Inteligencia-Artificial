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







;; Stats

(defun criar-estatisticas (algoritmo 
                           tabuleiro 
                           ramificacao 
                           gerados 
                           expandidos 
                           penetrancia 
                           tempo 
                           caminho)
  "Cria uma lista com as estatisticas para comparacao de algoritmos"
  (list algoritmo tabuleiro ramificacao gerados expandidos penetrancia tempo caminho)
)

(defun gera-e-conta-sucessores (tab)
  "Gera os sucessores e devolve-os juntamente com a sua quantidade"
  (let ((sucessores (gera-sucessores tab)))
    (values sucessores (length sucessores)))
)


(defun stats-bfs (inicial)
  "Algoritmo de procura BFS, devolve estatisticas"
  (let ((tempo-inicial (get-internal-real-time)))

    (multiple-value-bind (caminho gerados expandidos)
        (stats-bfs-aux (list (list inicial '())) '() 0 0)

      (let* ((tempo-final (get-internal-real-time))
             (tempo-ms 
              (/ (* (- tempo-final tempo-inicial) 1000) internal-time-units-per-second))
             (profundidade (length caminho))
             (ramificacao (if (zerop expandidos) 0 (/ gerados expandidos)))
             (penetrancia (if (zerop gerados)    0 (/ profundidade gerados))))

        (criar-estatisticas "BFS" inicial ramificacao gerados expandidos 
                            penetrancia tempo-ms caminho))))
)

(defun stats-bfs-aux (fila visitados gerados expandidos)
  "Funcao auxiliar do algoritmo bfs, com estatisticas"
  (cond ((null fila) (values nil gerados expandidos))

        ;; Extrai o primeiro no da fila
        (t (let* ((nodo (first fila))
                  (estado (first nodo))
                  (caminho (second nodo)))
             
             ;; Se o estado ja foi visitado continua com o resto da fila 
             (if (estado-visitado? estado visitados)
                 (stats-bfs-aux (rest fila) visitados gerados expandidos)
               
               ;; Incrementar nos expandidos
               (let ((exps (+ expandidos 1)))

                 ;; Se o objetivo for alcancado
                 (if (objetivo? estado)
                     (values (reverse caminho) gerados exps)

                   (multiple-value-bind (sucs novos-gerados)
                       (gera-e-conta-sucessores estado)
                     
                     ;; Incrementar nos gerados
                     (let ((gers (+ gerados novos-gerados)))
                       
                       ;; Remove primeiro elemento da fila e adiciona sucessores ao fim
                       (stats-bfs-aux
                        (append (rest fila)
                                (mapcar (lambda (par)
                                          (list (rest par)
                                                (cons (first par) caminho)))
                                        sucs))
                        (cons estado visitados)
                        gers
                        exps)))))))))
)


;;;Algoritmo  DFS COM LIMITE DE PROFUNDIDADE

(defun dfs (estado limite)
  "Algoritmo DFS com limite, devolve estatisticas"
  (let ((tempo-inicial (get-internal-real-time)))

    (multiple-value-bind (caminho gerados expandidos)
        (dfs-aux estado '() 0 limite '() 0 0)

      (let* ((tempo-final (get-internal-real-time))
             (tempo-ms 
              (/ (* (- tempo-final tempo-inicial) 1000) internal-time-units-per-second))
             (profundidade (length caminho))
             (ramificacao (if (zerop expandidos) 0 (/ gerados expandidos)))
             (penetrancia (if (zerop gerados)    0 (/ profundidade gerados))))

        (criar-estatisticas "DFS" estado ramificacao gerados expandidos 
                            penetrancia tempo-ms caminho))))
)

(defun dfs-aux (estado caminho prof limite visitados gerados expandidos)
  "Funcao auxiliar do algoritmo dfs, com estatisticas"
  (cond ((objetivo? estado)
         (values (reverse caminho) gerados expandidos))

    ;; Sinaliza que parou por profundidade nao por falha total
    ((>= prof limite)
     (values nil gerados expandidos :limit))

    ;; Estado ja visitado
    ((estado-visitado? estado visitados)
     (values nil gerados expandidos :fail))

    (t
     ;; Gerar sucessores
     (multiple-value-bind (sucessores novos-gerados)
             (gera-e-conta-sucessores estado)

           ;; Incrementar contadores
           (let ((gers (+ gerados novos-gerados))
                 (exps (+ expandidos 1)))
             
             ;; Explorar lista de sucessores
             (dfs-l-list sucessores caminho prof limite
                   (cons estado visitados) gers exps)))))
)


(defun dfs-l-list (lista-sucessores caminho prof limite visitados gerados expandidos)
  "Percorre, recursivamente, a lista de sucessores, tentando cada sucessor pela ordem"
  (cond
   ;; 1) Se nao ha sucessores restantes, falha (nesta ramificação não encontrou solução).
    ((null lista-sucessores)
     (values nil gerados expandidos :fail))

    ;; 2) Caso contrario, processa o primeiro (first) e prepara dados para explorar
    (t (let* ((par (first lista-sucessores))
              (mov (first par))
              (novo-estado (rest par))
              (novo-caminho (cons mov caminho)))

       ;; 3) Chama dfs-aux no sucessor atual, aumentando a profundidade (prof + 1)
       (multiple-value-bind (caminho gers exps resultado)
              (dfs-aux novo-estado novo-caminho (+ prof 1) 
                       limite visitados gerados expandidos)

         ;; 4) Se o resultado NAO for :fail (ou for :limit), devolve-o
         (if (or (not (eq resultado :fail)) (eq resultado :limit))
             (values caminho gers exps resultado)

           ;; 5) Caso contrario (resultado = :fail), tenta o próximo sucessor da lista
           (dfs-l-list (rest lista-sucessores) caminho 
                       prof limite visitados gers exps))))))
)









;;; A*

(defun a* (inicial h)
  "Algoritmo A*, devolve estatisticas"
  (let ((tempo-inicial (get-internal-real-time)))

    (multiple-value-bind (caminho gerados expandidos)
        (a*-aux (list (list inicial '() 0 h)) '() 0 0)

      (let* ((tempo-final (get-internal-real-time))
             (tempo-ms 
              (/ (* (- tempo-final tempo-inicial) 1000) internal-time-units-per-second))
             (profundidade (length caminho))
             (ramificacao (if (zerop expandidos) 0 (/ gerados expandidos)))
             (penetrancia (if (zerop gerados)    0 (/ profundidade gerados))))

        (criar-estatisticas "A*" inicial ramificacao gerados expandidos 
                            penetrancia tempo-ms caminho))))
)


(defun a*-aux (abertos fechados gerados expandidos)
  "Funcao auxiliar do algoritmo A*, com estatisticas"
  ;; Se nao ha mais nos a expandir falha
  (cond ((null abertos)
         (values nil gerados expandidos :fail))

        ;; Ordenar por f = g + h
        (t (let* ((ordenados
                   (sort (copy-list abertos)
                         (lambda (n1 n2)
                           (< (+ (third n1) (fourth n1))  ; g + h
                              (+ (third n2) (fourth n2))))))

                  (nodo (first ordenados))
                  (estado (first nodo))
                  (caminho (second nodo))
                  (g (third nodo)))

             ;; Se estado ja explorado, ignora
             (if (estado-visitado? estado fechados)
                 (a*-aux (rest ordenados) fechados gerados expandidos)

               ;; Se objetivo  devolve caminho
               (let ((exps (+ expandidos 1)))
                 (if (objetivo? estado)
                     (values (reverse caminho) gerados exps)

                   ;; Gerar sucessores
                   (multiple-value-bind (sucessores novos-gerados)
                       (gera-e-conta-sucessores estado)
           
                     (let ((gers (+ gerados novos-gerados))
                           (novos-nos
                            (mapcar
                             (lambda (par)
                               (let ((mov (first par))
                                     (novo-est (rest par)))
                                 (list novo-est
                                       (cons mov caminho)
                                       (+ g 1)
                                       (h2 novo-est))))
                             sucessores)))

                       ;; Recursao com:
                       ;; - Adicionar sucessores aos abertos
                       ;; - Estado atual passa para fechados
                       (a*-aux
                        (append (rest ordenados) novos-nos)
                        (cons estado fechados) gers exps)))))))))
)

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
  "Algoritmo IDA*, devolve estatisticas"
  (let ((tempo-inicial (get-internal-real-time))
        (limite h))

  (multiple-value-bind (caminho gerados expandidos)
    (ida*-iter inicial limite h 0 0)

  (let* ((tempo-final (get-internal-real-time))
             (tempo-ms (/ (* (- tempo-final tempo-inicial) 1000)
                          internal-time-units-per-second))
             (profundidade (length caminho))
             (ramificacao (if (zerop expandidos) 0 (/ gerados expandidos)))
             (penetrancia (if (zerop gerados) 0 (/ profundidade gerados))))

        (criar-estatisticas "IDA*" inicial ramificacao gerados expandidos
                            penetrancia tempo-ms caminho))))
)


;;Iterador do IDA*

(defun ida*-iter (estado limite h gerados expandidos)
  ;; Executa um DFS limitado
  (multiple-value-bind (caminho gers exps resultado)
      (ida*-dfs estado '() 0 h limite '() gerados expandidos)

    ;; Caminho encontrado
    (cond ((eq resultado :ok)
           (values caminho gers exps :ok))

      ;; Caminho nao encontrado e resultado infinito
      ((= resultado most-positive-fixnum)
       (values nil gers exps :fail))

      ;; Caso contrario, aumentar limite e repetir
      (t (ida*-iter estado resultado h gers exps))))
)


;;DFS do IDA*

(defun ida*-dfs (estado caminho g h limite visitados gerados expandidos)
  ;; Custo total
  (let ((f (+ g h)))

    ;; 1. Se f > limite  este estado nao pode ser expandido
    (cond ((> f limite)
           (values nil gerados expandidos f))

          ;; 2. Objetivo encontrado
          ((objetivo? estado)
           (values (reverse caminho) gerados expandidos :ok))

          ;; 3. Estado repetido evita ciclos
          ((estado-visitado? estado visitados)
           (values nil gerados expandidos most-positive-fixnum))

          (t
           ;; 4. Explora sucessores
           (multiple-value-bind (sucessores novos-gerados)
               (gera-e-conta-sucessores estado)

             (let ((gers (+ gerados novos-gerados))
                   (exps (+ expandidos 1)))

               ;; Expandir sucessores
               (ida*-dfs-expand sucessores caminho g h limite (cons estado visitados) 
                                gers exps most-positive-fixnum))))))
)


;; Expansao dos sucessores, um por um

(defun ida*-dfs-expand (sucessores caminho g h limite visitados gerados expandidos melhor-ultrapassou)
  (cond
   ;; Sem sucessores: Devolve o melhor limite ultrapassado ate agora
   ((null sucessores)
    (values nil gerados expandidos melhor-ultrapassou))

   (t
    ;; Extrair sucessor
    (let* ((par (first sucessores))
           (mov (first par))
           (novo-est (rest par))
           (novo-caminho (cons mov caminho)))

      (multiple-value-bind (cam gers exps resultado)
          (ida*-dfs novo-est
                    novo-caminho
                    (+ g 1)
                    h
                    limite
                    visitados
                    gerados
                    expandidos)

    ;; Se encontrou caminho devolve caminho
    (if (eq resultado :ok)
        (values cam gers exps :ok)

      ;; Caso contrario, atualiza o melhor ultrapassado e continua
      (ida*-dfs-expand (rest sucessores)
                       caminho
                       g
                       h
                       limite
                       visitados
                       gers
                       exps
                       (min melhor-ultrapassou resultado)))))))
)


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
     (lambda (par)
       (let* ((mov (first par))
              (novo (rest par))
              (ng (+ g 1))
              (nh (h2 novo)))
         (make-no novo (cons mov caminho) ng nh most-positive-fixnum)))
     pares)))


(defun stats-expandir-no (no gerados expandidos)
  "Funcao expandir-no, mas devolve estatisticas"
  (let* ((estado (no-estado no))
         (caminho (no-caminho no))
         (g (no-g no)))

    (multiple-value-bind (pares novos-gerados)
        (gera-e-conta-sucessores estado)
      
      (let ((gers (+ gerados novos-gerados)))
        (let ((filhos   
               (mapcar
                (lambda (par)
                  (let* ((mov (first par))
                         (novo (rest par))
                         (ng (+ g 1))
                         (nh (h2 novo)))
                    (make-no novo (cons mov caminho) ng nh most-positive-fixnum)))
                pares)))
          
          (values filhos gers expandidos)))))
)


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

(defun sma*-aux (abertos max-nos gerados expandidos)
  (cond ((null abertos) 
         (values nil gerados expandidos :fail))

        (t 
         ;; Escolhe o nó com menor f (f = g + h)
         (let* ((ordenados (ordenar-abertos abertos))
                (n0 (first ordenados))
                (resto (rest ordenados)))

           ;; Caso objetivo encontrado
           (if (objetivo? (no-estado n0))
               (values (reverse (no-caminho n0)) gerados expandidos)

             ;; Expandir no
             (multiple-value-bind (filhos novos-gerados novos-expandidos)
                 (stats-expandir-no n0 gerados expandidos)

               (let ((gers novos-gerados)
                     (exps (+ novos-expandidos 1)))

                 ;; Nova lista com filhos + resto
                 (let* ((novos (append filhos resto))
                        (limitados (cortar-ate novos max-nos)))

                 ;; Recursao SMA*
                 (sma*-aux limitados max-nos gers exps))))))))
)


;; Interface: recebe estado inicial e máximo de nós permitidos

(defun sma* (inicial max-nos h)
  "Algoritmo SMA*, devolve estatisticas"
  (let ((tempo-inicial (get-internal-real-time))
        (root (make-no inicial '() 0 h h)))

    (multiple-value-bind (caminho gerados expandidos)
        (sma*-aux (list root) max-nos 0 0)

    (let* ((tempo-final (get-internal-real-time))
           (tempo-ms (/ (* (- tempo-final tempo-inicial) 1000)
                        internal-time-units-per-second))
           (profundidade (length caminho))
           (ramificacao (if (zerop expandidos) 0 (/ gerados expandidos)))
           (penetrancia (if (zerop gerados) 0 (/ profundidade gerados))))

      (criar-estatisticas "SMA*" inicial ramificacao gerados expandidos
                          penetrancia tempo-ms caminho))))
)
