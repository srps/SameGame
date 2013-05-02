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

; Atribui ou junta blocos a partir da peça à direita no tabuleiro
; TODO: mudar nome da função, adicionar efeito dominó quando encontra uma peça da mesma cor com outro bloco associado

(defun ve-frente (tabuleiro p-aux b-aux posx posy hash)
  (let* ((l-aux (bloco-lista-pecas b-aux))
         (p-dir (nth (+ posx 1) (nth posy tabuleiro))))
    (print "entrou: ve-frente")
    (if (= (peca-cor p-aux) (peca-cor p-dir))                               ; Se o da frente for igual
        (progn
         (format t "Right Match On: posx: ~D posy: ~D ~% " posx posy)
         (setf (peca-bloco p-dir) (peca-bloco p-aux))                       ; Junta a informação do bloco à peça da direita
         (setf (nth (+ posx 1) (nth posy tabuleiro)) p-dir)                 ; Coloca a peça atualizada no tabuleiro
         (setq l-aux (append l-aux (list p-dir)))                           ; Adiciona a peça à lista para atualizar o bloco
         (setf (bloco-lista-pecas (gethash (peca-bloco p-dir) hash)) l-aux) ; Atualiza o bloco na hash
))))

; Atribui ou junta blocos a partir da peça por baixo no tabuleiro
; TODO: mudar nome da função, adicionar efeito dominó quando encontra uma peça da mesma cor com outro bloco associado

(defun ve-abaixo (tabuleiro p-aux b-aux posx posy hash)
  (let* ((l-aux (bloco-lista-pecas b-aux))
         (p-baixo (nth posx (nth (+ posy 1) tabuleiro))))
    (print "entrou: ve-abaixo")
    (if (= (peca-cor p-aux) (peca-cor p-baixo))                                    ; Se o da frente for igual
        (progn
          (format t "Down Match On: posx: ~D posy: ~D ~% " posx posy)              
          (setf (peca-bloco p-baixo) (peca-bloco p-aux))                           ; Junta a informação do bloco à peça da direita
          (setf (nth posx (nth (+ posy 1) tabuleiro)) p-baixo)                     ; Coloca a peça atualizada no tabuleiro
          (setq l-aux (append l-aux (list p-baixo)))                               ; Adiciona a peça à lista para atualizar o bloco
          (setf (bloco-lista-pecas (gethash (peca-bloco p-baixo) hash)) l-aux))))) ; Atualiza o bloco na hash

; Cria o tabuleiro principal com as peças
; TODO: adicionar a funcionalidade de atribuir os blocos iniciais logo aqui, tbh.

(defun cria-tabuleiro (tabuleiro n-col)
  (let* ((resul (list))
         (posx 0)
         (posy 0)
         (p-aux)
         (l-aux (list)))
    (print "entrou: cria-tabuleiro")
    (loop for linha in tabuleiro do
          (loop for coluna in linha do
                (setq p-aux (make-peca :pos (cons posx posy) :cor coluna :bloco NIL))
                (setq l-aux (append l-aux (list p-aux)))
                (if (not (= posx (- n-col 1))) ; Avança no Y caso não esteja no final da linha
                    (incf posx)               
                  (progn                       ; Se estiver no Final, desce 1 linha e faz reset no posx
                    (setq resul (append resul (list l-aux)))
                    (setq l-aux (list))
                    (setf posx 0)          
                (incf posy)))))
                resul))
                      
; Percorre a área indicada do tabuleiro e verifica os blocos das peças em questão

(defun lista-blocos (tabuleiro x-ini x-fin y-ini y-fin n-lin n-col hash)
  (let* ((resul hash)
         (contador 0)
         (p-aux (make-peca))
         (b-aux (make-bloco)))
    (print "entrou: lista-blocos")
    (loop for posy from y-ini to y-fin do
          (loop for posx from x-ini to x-fin do
                (setq p-aux (nth posx (nth posy tabuleiro)))
                (if (not (peca-bloco p-aux))                             ; Esta cena é alto desperdício de recursos IMO. 
                    (progn                                               ; Só à primeira passagem é que as peças não vão ter bloco associado. Devia passar para o cria-tabuleiro que só corre uma vez.
                      (setf (peca-bloco p-aux) contador)
                      (setq b-aux (make-bloco :cor (peca-cor p-aux) :lista-pecas (list p-aux) :id contador))
                       (setf (gethash (peca-bloco p-aux) resul) b-aux)
                      (setf (nth posx (nth posy tabuleiro)) p-aux) 
                      (incf contador))
                    (setf b-aux (gethash (peca-bloco p-aux) resul)))     ; Daqui por mim só ficava isto.
                (if (not (>= posx (- n-col 1)))
                    (ve-frente tabuleiro p-aux b-aux posx posy resul))   ; Vê se bloco à direita é da mesma cor
                (if (not (>= posy (- n-lin 1)))
                    (ve-abaixo tabuleiro p-aux b-aux posx posy resul)))) ; Vê se bloco em baixo é da mesma cor   
    resul))


; Funcao principal. Ponto de Entrada

(defun resolve-same-game (problema algoritmo)
  (let* ((tab (cria-tabuleiro problema (list-length (first problema))))
         (h-blocos (lista-blocos tab 0 (- (list-length (first problema)) 1) 0 (- (list-length problema) 1) (list-length problema) (list-length (first problema)) (make-hash-table)))
         (estado-inicial (make-no :n-pecas (* (list-length problema) (list-length (first problema))) :n-blocos (hash-table-count h-blocos) :tabuleiro tab :l-blocos h-blocos))
        ; (gera-sucessores	#'sucessores)
        ; (heuristica1		#'heur-melhor-primeiro)
        ; (heuristica2		#'heur-melhor-primeiro-posicao-menor)
        ; (heuristica-opt	#'heur-menor-altura)
         resul solucao)
  estado-inicial)
)


(print (resolve-same-game '((1 1 1 10 8) (1 2 2 1 3) (1 2 2 2 2) (1 1 1 1 1))
 "melhor.abordagem.optimizacao"))


