;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;																	;;;
;;;			Procura e Planeamento 2012-2013                 ;;;
;;;			Projecto Same Game	                        ;;;
;;;									;;;
;;;			Grupo 002				        ;;;
;;;			55457 - Sergio Miguel Silva			;;;
;;;			56886 - Marco Andre Ferreira			;;;
;;;									;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :user)

;(build-lisp-image "biggerimage.dxl" :lisp-heap-start "256m"
;;;                  :c-heap-start "1610m")

(eval-when (compile) (declaim (optimize (speed 3) (safety 0) (debug 0))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;	DEFINICOES  e  ESTRUTURAS  DE  DADOS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defstruct peca
  pos
  cor
  bloco
)

(defstruct bloco
  cor
  lista-pecas
  pecas-int             ; peças interiores
  pecas-ext             ; peças exteriores
  id
)

	
(defstruct no
  (pontuacao 0	:type fixnum) ; Pontuação até ao momento do estado
  n-pecas	              ; Peças por eliminar
  n-blocos	              ; Blocos por eliminar         
  tabuleiro
  h-blocos                    ; Hash com os blocos existentes		
)

; Funcao principal. Ponto de Entrada


(defun ve-frente (tabuleiro p-aux b-aux posx posy hash)
  (let* ((l-aux (bloco-lista-pecas b-aux))
         (p-dir (nth (+ posx 1) (nth posy tabuleiro))))
  (if (= (peca-cor p-aux) (peca-cor p-dir)) ; Se o da frente for igual
       (progn
       (format t "Right Match On: posx: ~D posy: ~D ~% " posx posy)
       (setf (peca-bloco p-dir) (peca-bloco p-aux))
       (setf (nth posy (nth (+ posx 1) tabuleiro)) p-dir)
       (setq l-aux (append l-aux (list p-dir)))
       (setf (bloco-lista-pecas (gethash (peca-bloco (nth posy (nth posx tabuleiro))) hash)) l-aux)
))))

(defun ve-abaixo (tabuleiro p-aux b-aux posx posy hash)
  (let* ((l-aux (bloco-lista-pecas b-aux))
         (p-baixo (nth posx (nth (+ posy 1) tabuleiro))))
  (if (= (peca-cor p-aux) (peca-cor p-baixo)) ; Se o da frente for igual
       (progn
       (format t "Down Match On: posx: ~D posy: ~D ~% " posx posy)
       (setf (peca-bloco p-baixo) (peca-bloco p-aux))
       (setf (nth (+ posy 1) (nth posx tabuleiro)) p-baixo)
       (setq l-aux (append l-aux (list p-baixo)))
       (setf (bloco-lista-pecas (gethash (peca-bloco (nth posx (nth posy tabuleiro))) hash)) l-aux)))))

(defun cria-tabuleiro (tabuleiro n-col)
  (let* ((resul (list))
         (posx 0)
         (posy 0)
         (l-aux (list)))
    (loop for linha in tabuleiro do
          (loop for coluna in linha do
                (setq peca (make-peca :pos (cons posx posy) :cor coluna :bloco NIL))
                (setq l-aux (append l-aux (list peca)))
                (if (not (= posx (- n-col 1))) ; Avança no Y caso não esteja no final da linha
                    (incf posx)               
                  (progn                       ; Se estiver no Final, desce 1 linha e faz reset no posx
                    (setq resul (append resul (list l-aux)))
                    (setq l-aux (list))
                    (setf posx 0)          
                (incf posy)))))
                resul))
                      

(defun lista-blocos (tabuleiro x-ini x-fin y-ini y-fin n-lin n-col hash)
  (let* ((resul hash)
         (contador 0)
         (p-aux (make-peca))
         (b-aux (make-bloco)))
    (loop for posy from y-ini to y-fin do
          (loop for posx from x-ini to x-fin do
                (print "1")
                (setq p-aux (nth posy (nth posx tabuleiro)))
                (print "3")
                (print p-aux)
                (print (not (peca-bloco p-aux)))
                 
                (if (not (peca-bloco p-aux))
                    (progn  
                       (print "4")
                      (setf (peca-bloco p-aux) contador)
                      (print "4")
                      (setq b-aux (make-bloco :cor p-aux-cor :lista-pecas (list p-aux) :id contador))
                       (print "5")
                      (setf (nth posy (nth posx tabuleiro)) p-aux) 
                       (print "6")
                      (incf contador))
                    (setf b-aux (gethash (peca-bloco p-aux) resul)))
                (print "2")
                (if (not (= posx (- n-col 1)))
                    (ve-frente tabuleiro p-aux b-aux posx posy resul)) ; Ve se bloco à direita é da mesma cor
                (if (not (= posy (- n-lin 1)))
                    (ve-abaixo tabuleiro p-aux b-aux posx posy resul)))) ; Ve se bloco em baixo é da mesma cor   
    (print "COF")
    (print tabuleiro)
    resul))

(defun resolve-same-game (problema algoritmo)
  (let* ((tab (cria-tabuleiro problema (list-length (first problema))))
         (h-blocos (lista-blocos tab 0 (list-length problema) 0 (list-length (first problema)) (list-length problema) (list-length (first problema)) (make-hash-table)))
         (estado-inicial (make-no :n-pecas (* (list-length problema) (list-length (first problema))) :n-blocos (list-length h-blocos) :tabuleiro tab :l-blocos h-blocos))
        ; (gera-sucessores	#'sucessores)
        ; (heuristica1		#'heur-melhor-primeiro)
        ; (heuristica2		#'heur-melhor-primeiro-posicao-menor)
        ; (heuristica-opt	#'heur-menor-altura)
         resul solucao)
  (print tab))
)


(print (resolve-same-game '((1 1 2 10 8) (1 2 2 1 3) (1 2 2 2 2) (1 1 1 1 1))
 "melhor.abordagem.optimizacao"))


