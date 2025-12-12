;;; Projeto Solitario - Inteligencia Artificial 2025/2026
;;; Autores: 
;; Felisberto de Carvalho 202200359
;; Tiago Campos 202300064
;; Filipe Patricio 202300133

;;; projeto.lisp
;;; Interface principal com o utilizador
;;; Carrega: puzzle.lisp e procura.lisp
;;; Lê: problemas.dat


;; Fazer (setf *default-pathname-defaults* #P"C:/PastaProjeto/") no Listener

(defun carregar-ficheiros ()
  "Carrega os ficheiros puzzle.lisp e procura.lisp."
  (load "puzzle.lisp")
  (load "procura.lisp")
)


;; Ler e imprimir ficheiro problemas.dat

(defun ler-problemas ()
  "Le os problemas do ficheiro problemas.dat."
    (with-open-file (stream "problemas.dat")
      (loop for problema = (read stream nil)
            while problema
            collect problema))
)

(defun print-problemas ()
  "Imprime a lista dos tabuleiros disponiveis."
  (format t "Tabuleiros disponíveis: ~%")
  (loop for i from 1 to (length (ler-problemas)) do
        (format t "~A -> Tabuleiro ~A~%" i i))
)


;; Guardar resultados no ficheiro resultados.dat

(defun guardar-estatisticas (stats)
  (with-open-file (str "resultados.dat"
                       :direction :output
                       :if-exists :append
                       :if-does-not-exist :create)
    (format str "--- Solucao Encontrada ---~%")
    (format str "Algoritmo: ~A~%" (stats-algoritmo stats))
    (format str "Tabuleiro: ~A~%" (stats-tabuleiro stats))
    (format str "~%--- Estatisticas ---~%")
    (format str "Fator de ramificacao: ~A~%" (stats-ramificacao stats))
    (format str "Nos gerados: ~A~%" (stats-nos-gerados stats))
    (format str "Nos expandidos: ~A~%" (stats-nos-expandidos stats))
    (format str "Penetrancia: ~A~%" (stats-penetrancia stats))
    (format str "Tempo (ms): ~A~%" (stats-tempo stats))
    (format str "Caminho: ~A~%~%~%~%" (stats-caminho stats)))
)


;; Listas de escolhas

(defun menu-principal ()
  "Imprime o menu principal que inicia a interacao com o utilizador."
  (format t "~%--- Bem-vindo ao jogo do Solitario! ---~%")
  (format t "1 - Jogar Solitario~%")
  (format t "2 - Sair~%")
  (format t "> ")
  (read)
)

(defun escolher-problema ()
  "Permite ao utilizador escolher um problema pelo indice e devolve o tabuleiro escolhido."
  (print-problemas)
  (format t "Escolha um problema: ")
  (let ((option (read)))
    (nth (1- option) (ler-problemas)))
)

(defun escolher-algoritmo ()
  "Imprime a lista de algoritmos disponiveis e espera pela escolha do utilizador."
  (format t "~%- Escolha um algoritmo: ~%")
  (format t "1 - BFS~%")
  (format t "2 - DFS~%")
  (format t "3 - A*~%")
  (format t "4 - IDA*~%")
  (format t "5 - SMA*~%")
  (format t "> ")
  (read)
)

(defun escolher-heuristica ()
  "Imprime a lista de heuristicas disponiveis e espera pela escolha do utilizador."
  (format t "~%- Escolha uma heuristica: ~%")
  (format t "1 - Heuristica 1~%")
  (format t "2 - Heuristica 2~%")
  (format t "> ")
  (read)
)


;; Loop principal

(defun main ()
  "Loop principal do programa, imprime o menu principal."
  (carregar-ficheiros)
  (loop
     (let ((option (menu-principal)))
       (cond ((= option 1) 
              (let* (
                  (problema (escolher-problema)) 
                  (algoritmo (escolher-algoritmo)) 
                  (heuristica (if (> algoritmo 2)
                                  (escolher-heuristica)
                                   nil))
                  (resultado (executar-procura problema algoritmo heuristica))
                )
                (mostrar-resultado resultado)
                (guardar-estatisticas resultado)
                ))
             ((= option 2)(format t "A sair do jogo...~%")
              (return))
             (t (format t "Opcao invalida!~%")))))
)


;; Executar algoritmos

(defun executar-procura (tabuleiro algoritmo heuristica)
  "Executa o algoritmo escolhido pelo utilizador num dos tabuleiros."
  (cond ((= algoritmo 1) (stats-bfs tabuleiro))

        ((= algoritmo 2) (dfs tabuleiro 30))

        ((= algoritmo 3) 
         (cond ((= heuristica 1)(a* tabuleiro (h1 tabuleiro)))
               (t (a* tabuleiro (h2 tabuleiro)))))

        ((= algoritmo 4) 
         (cond ((= heuristica 1)(ida* tabuleiro (h1 tabuleiro)))
               (t (ida* tabuleiro (h2 tabuleiro)))))

        ((= algoritmo 5) 
         (cond ((= heuristica 1)(sma* tabuleiro 30 (h1 tabuleiro)))
               (t (sma* tabuleiro 30 (h2 tabuleiro)))))

        (t (format t "Algoritmo invalido!~%")))
)

(defun mostrar-resultado (resultado)
  "Mostra a solucao da procura no ecra."
  (format t "-> Solucao encontrada: ~A~%" (last resultado))
)


;; Ferramentas

;; Acesso as estatisticas

(defun stats-algoritmo (stats)
  (nth 0 stats)
)

(defun stats-tabuleiro (stats)
  (nth 1 stats)
)

(defun stats-ramificacao (stats)
  (nth 2 stats)
)

(defun stats-nos-gerados (stats)
  (nth 3 stats)
)

(defun stats-nos-expandidos (stats)
  (nth 4 stats)
)

(defun stats-penetrancia (stats)
  (nth 5 stats)
)

(defun stats-tempo (stats)
  (nth 6 stats)
)

(defun stats-caminho (stats)
  (nth 7 stats)
)


(defun read-int (prompt &optional min max)
  "Le um inteiro com prompt; valida intervalo se min/max fornecidos."
  (format t "~a" prompt)
  (let ((v (read)))
    (if (and (integerp v) (or (null min) (>= v min)) (or (null max) (<= v max)))
        v
        (progn (format t "Entrada invalida.~%")
               (read-int prompt min max)))))

(defun read-str (prompt)
  (format t "~a" prompt)
  (finish-output)
  (let ((s (read-line)))
    (if s s (read-str prompt))))

(defun yes-no-p (prompt)
  (format t "~a (s/n): " prompt)
  (let ((r (string-downcase (read-line))))
    (cond ((or (string= r "s") (string= r "sim")) t)
          ((or (string= r "n") (string= r "nao") (string= r "nao")) nil)
          (t (format t "Resposta invalida.~%") (yes-no-p prompt)))))
