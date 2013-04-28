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


(defun ve-frente (tabuleiro posx posy)
   (if (= (nth (+ posx 1) (nth posy tabuleiro)) (nth posx (nth posy tabuleiro))) ; Se o da frente for igual
       (format t "Right Match On: posx: ~D posy: ~D ~% " posx posy))
)

(defun ve-abaixo (tabuleiro posx posy)
  (if (= (nth posx (nth posy tabuleiro)) (nth posx (nth (+ 1 posy) tabuleiro))) ; Se o de baixo for igual
      (format t  "Bottom Match On: posx: ~D posy: ~D ~% " posx posy))
)

(defun cria-tabuleiro (tabuleiro n-lin n-col)
  (let* ((resul (list))
         (posx 0)
         (posy 0)
         (l-aux (list)))
    (loop for linha in tabuleiro do
          (loop for coluna in linha do
                (setq peca (make-peca :pos (cons posx posy) :cor coluna :bloco NIL))
                (setq l-aux (append l-aux (list peca)))
                ;(print first l-aux)
                (if (not (= posx (- n-col 1))) ; Avança no Y caso não esteja no final da linha
                    (incf posx)               
                  (progn                       ; Se estiver no Final, desce 1 linha e faz reset no posx
                    (setq resul (append resul (list l-aux)))
                    (setq l-aux '())
                    (setf posx 0)          
                (incf posy)))))
                resul))
                      

(defun lista-blocos (tabuleiro n-lin n-col hash)
  (let* ((resul (list '()))
         (posx 0)
         (posy 0)
         (contador 0))
    (loop for linha in tabuleiro do
          (loop for coluna in linha do
                (setq peca (make-peca :pos (cons posx posy) :cor coluna :bloco contador))
                (setq bloco (make-bloco :cor coluna :lista-pecas (list peca) :id contador))
                (incf contador)
                (print linha)
                (print coluna)
                (if (not (= posx (- n-col 1)))
                    (ve-frente tabuleiro posx posy)) ; Ve se bloco à direita é da mesma cor
                (if (not (= posy (- n-lin 1)))
                    (ve-abaixo tabuleiro posx posy)) ; Ve se bloco em baixo é da mesma cor
 
   (print teste)
    
    
    (if (not (= posx (- n-col 1))) ; Avança no Y caso não esteja no final da linha
      (incf posx)               
      (progn                       ; Se estiver no Final, desce 1 linha e faz reset no posx
        (setf posx 0)          
        (incf posy)))
    ))
    (print tabuleiro)
))

(defun resolve-same-game (problema algoritmo)
  (let* ((tab (cria-tabuleiro problema (list-length problema) (list-length (first problema))))
         (h-blocos (lista-blocos problema (list-length problema) (list-length (first problema)) (make-hash-table)))
         (estado-inicial (make-no :n-pecas (* (list-length problema) (list-length (first problema))) :n-blocos (list-length h-blocos) :tabuleiro problema :l-blocos h-blocos))
        ; (gera-sucessores	#'sucessores)
        ; (heuristica1		#'heur-melhor-primeiro)
        ; (heuristica2		#'heur-melhor-primeiro-posicao-menor)
        ; (heuristica-opt	#'heur-menor-altura)
         resul solucao)
  (print tab))
)


(print (resolve-same-game '((1 2 2 10 8) (1 2 2 1 3) (1 2 2 2 2) (1 1 1 1 1))
 "melhor.abordagem.optimizacao"))


