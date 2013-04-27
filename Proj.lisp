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
  lista-blocos                ; Lista dos blocos existentes		
)

; Funcao principal. Ponto de Entrada


(defun check-right (tabuleiro posx posy)
   (if (= (nth (+ posx 1) (nth posy tabuleiro)) (nth posx (nth posy tabuleiro))) ; Se o da frente for igual
             (format t "Right Match On: posx: ~D posy: ~D ~% " posx posy) )
)

(defun check-bottom (tabuleiro posx posy)
  (if  (= (nth posx (nth posy tabuleiro)) (nth posx (nth (+ 1 posy) tabuleiro))) ; Se o de baixo for igual
      (format t  "Bottom Match On: posx: ~D posy: ~D ~% " posx posy))
)

(defun lista-blocos (tabuleiro n-lin n-col hash)
  (let* ((resul (list '()))
        (posx 0)
        (posy 0)
        (contador 0))
    (loop for linha in tabuleiro do
     (loop for coluna in linha do
    (setq bloco (make-bloco :cor (nth posx (nth posy tabuleiro)) :lista-pecas (list posx posy)))
    (print linha)
    (print coluna)
    (if (not (= posx (- n-col 1)))
    (check-right tabuleiro posx posy)) ; Ve se bloco à direita é da mesma cor
    (if (not (= posy (- n-lin 1)))
    (check-bottom tabuleiro posx posy)) ; Ve se bloco em baixo é da mesma cor
 
   
    
    
    (if (not (= posx (- n-col 1))) ; Avança no Y caso não esteja no final da linha
      (incf posx)               
      (progn                       ; Se estiver no Final, desce 1 linha e faz reset no posx
        (setf posx 0)          
        (incf posy)))
    ))
    (print tabuleiro)
))

(defun resolve-same-game (problema algoritmo)
  (let* ((hash-blocos (lista-blocos problema (list-length problema) (list-length (first problema)) (make-hash-table)))
         (estado-inicial (make-no :n-pecas (* (list-length problema) (list-length (first problema))) :n-blocos (list-length blocos) :tabuleiro problema :l-blocos hash-blocos))
        ; (gera-sucessores	#'sucessores)
        ; (heuristica1		#'heur-melhor-primeiro)
        ; (heuristica2		#'heur-melhor-primeiro-posicao-menor)
        ; (heuristica-opt	#'heur-menor-altura)
         resul solucao)
  (print "yay"))
)


(print (resolve-same-game '((1 2 2 10 8) (1 2 2 1 3) (1 2 2 2 2) (1 1 1 1 1))
 "melhor.abordagem.optimizacao"))


