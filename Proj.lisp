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
  pos                         ; posi��o da pe�a (x . y)
  (cor -1 :type fixnum)       ; cor da pe�a
  (bloco -1 :type fixnum)     ; bloco a que pertence a pe�a
)

(defstruct bloco
  (cor -1 :type fixnum)       ; cor do bloco
  lista-pecas                 ; lista das pe�as do bloco
  pecas-int                   ; pe�as interiores
  pecas-ext                   ; pe�as exteriores
  (id -1 :type fixnum)        ; identificador do bloco
  (x-min 20 :type fixnum)
  (x-max -1 :type fixnum)
  (y-min 20 :type fixnum)
  (y-max -1 :type fixnum)
)

	
(defstruct no
  (pontuacao 0	:type fixnum) ; Pontua��o at� ao momento do estado
  (n-pecas 0 :type fixnum)    ; Pe�as por eliminar
  (n-blocos 0 :type fixnum)   ; Blocos por eliminar         
  tabuleiro
  h-blocos                    ; Hash com os blocos existentes	
  (n-linhas 0 :type fixnum)   ; Numero de linhas com pe�as
  (n-colunas 0 :type fixnum)  ; Numero de colunas com pe�as
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;	DEBUG
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun print-tabuleiro (tabuleiro n-lin n-col)
  (let* ((result '())
         (temp-list '()))
    (loop for posy from 0 to n-lin do
          (loop for posx from 0 to n-col do
                (setq temp-list (append temp-list (list (peca-cor (nth posx (nth posy tabuleiro))))))
                )
          (setq result (append result (list temp-list)))
          (setf temp-list '())
           )
  result)
)

(defun print-hash (hash)
  (let* ((b-aux (make-bloco)))
 (loop for key being the hash-keys of hash do
       (setf b-aux (gethash key hash))
       (format t "~% Key: ~D Cor Bloco: ~D Numero De Pe�as: ~D Xmin: ~D Xmax: ~D Ymin: ~D Ymax: ~D " key (bloco-cor b-aux) (list-length (bloco-lista-pecas b-aux)) (bloco-x-min b-aux) (bloco-x-max b-aux) (bloco-y-min b-aux) (bloco-y-max b-aux))
       )
 )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;	FUN��ES AUXILIARES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;--------------------------------------------------------------------------;
; Fun��o que remove bloco do tabuleiro e hash table e atualiza a pontua��o ;
;--------------------------------------------------------------------------;
; ARG1 - Estado (n�)                                                       ;
; ARG2 - Bloco a ser removido                                              ;
; ARG3 - Hash table                                                        ;
;--------------------------------------------------------------------------;

(defun remove-bloco (estado bloco ht)
  (let* ((l-aux (bloco-lista-pecas (gethash bloco ht)))
         (pontos (expt (- (list-length l-aux) 2) 2))
         (pos))
    (remhash bloco ht)
    (setf (no-pontuacao estado) (+ (no-pontuacao estado) pontos))
    (loop for p-aux in l-aux do
          (setq pos (peca-pos p-aux))
          (setf (nth (car pos) (nth (cdr pos) (no-tabuleiro estado))) NIL))))


;----------------------------------------;
; Fun��o que junta 2 blocos da mesma cor ;
; ---------------------------------------;
; ARG1 - tabuleiro com as pe�as          ;
; ARG2 - hashtable dos blocos            ;
; ARG3 - chave do bloco a manter         ;
; ARG4 - chave do bloco que desaparece   ;
;----------------------------------------;

(defun junta-blocos (tabuleiro ht chave-b1 chave-b2)
  (let* ((b-aux (gethash chave-b1 ht))                               ; Refer�ncia para o bloco que se vai manter
         (l-aux (bloco-lista-pecas b-aux))                           ; Lista das pe�as do bloco que se vai manter
         (posx)
         (posy))
    (print "entrou: junta-blocos")
  (loop for p-aux in (bloco-lista-pecas (gethash chave-b2 ht)) do
        (setq posx (car (peca-pos p-aux)))                           ; Guarda a coordenada x da pe�a a ser mudada
        (setq posy (cdr (peca-pos p-aux)))                           ; Guarda a coordenada y da pe�a a ser mudada
        (setf (peca-bloco p-aux) chave-b1)                           ; Muda o bloco da pe�a
        (setq l-aux (append l-aux (list p-aux)))                     ; Insere a pe�a na lista do bloco original
        (setf (nth posx (nth posy tabuleiro)) p-aux))                ; Re-insere a pe�a no tabuleiro
  (setf (bloco-lista-pecas b-aux) l-aux)                             ; Coloca a nova lista no bloco original
  (setf (gethash chave-b1 ht) b-aux)                                 ; Atualiza o bloco original na HT
  (remhash chave-b2 ht)))                                            ; Remove o 2� bloco da HT

;---------------------------------------------;
; Fun��o que percorre uma determinada zona    ;
; e determina os diferentes blocos existentes ;
; --------------------------------------------;
; ARG1 - tabuleiro com as pe�as               ;
; ARG2 - limite esquerda                      ;
; ARG3 - limite direita                       ;
; ARG4 - limite cima                          ;
; ARG5 - limite baixo                         ;
; ARG6 - numero linhas do tabuleiro           ;
; ARG7 - numero colunas do tabuleiro          ;
; ARG8 - hash com bloocs                      ;
;---------------------------------------------;

(defun lista-blocos (tabuleiro x-ini x-fin y-ini y-fin n-lin n-col hash)
  (let* ((resul hash)
         (p-aux (make-peca))
         (p-aux-dir (make-peca))
         (p-aux-baixo (make-peca)))
    (print "entrou: lista-blocos")
    (loop for posy from y-ini to y-fin do
          (loop for posx from x-ini to x-fin do
                (setq p-aux (nth posx (nth posy tabuleiro)))
                (if (not (>= posx (- n-col 1)))                                                               ; Estou na �ltima coluna do tabuleiro?
                    (progn                                                                                    ; Se n�o, v� � direita
                      (setq p-aux-dir (nth (+ 1 posx) (nth posy tabuleiro)))
                      (if (= (peca-cor p-aux) (peca-cor p-aux-dir))                                           ; Se pe�a � direita for da mesma cor
                          (if (not (= (peca-bloco p-aux) (peca-bloco p-aux-dir)))                             ; Se forem da mesma cor mas blocos diferentes
                              (junta-blocos tabuleiro hash (peca-bloco p-aux) (peca-bloco p-aux-dir))))))     ; Junta os blocos              
                (if (not (>= posy (- n-lin 1)))                                                               ; Estou na �ltima linha do tabuleiro?
                    (progn                                                                                    ; Se n�o, v� abaixo
                      (setq p-aux-baixo (nth posx (nth (+ 1 posy) tabuleiro))) 
                      (if (= (peca-cor p-aux) (peca-cor p-aux-baixo))                                         ; Se pe�a em baixo for da mesma cor
                          (if (not (= (peca-bloco p-aux) (peca-bloco p-aux-baixo)))                           ; Se forem da mesma cor mas blocos diferentes 
                              (junta-blocos tabuleiro hash (peca-bloco p-aux) (peca-bloco p-aux-baixo)))))))) ; Junta os blocos              
    resul))
 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;	FUN��ES DE LEITURA DE TABULEIRO INICIAL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;        

;--------------------------------------------------------------------;
; Fun��o que gera pe�as e constr�i um tabuleiro a partir do original ;
; -------------------------------------------------------------------;
; ARG1 - tabuleiro com lista de cores recebido no input              ;
; ARG2 - numero de colunas                                           ;
;--------------------------------------------------------------------;

(defun cria-tabuleiro (tabuleiro n-col)
  (let* ((resul (list))
         (posx 0)
         (posy 0)
         (p-aux)
         (l-aux (list)))
    (print "entrou: cria-tabuleiro")
    (loop for linha in tabuleiro do
          (loop for coluna in linha do
                (setq p-aux (make-peca :pos (cons posx posy) :cor coluna :bloco -1))
                (setq l-aux (append l-aux (list p-aux)))
                (if (not (= posx (- n-col 1)))                                       ; Avan�a no Y caso n�o esteja no final da linha
                    (incf posx)               
                  (progn                                                             ; Se estiver no Final, desce 1 linha e faz reset no posx
                    (setq resul (append resul (list l-aux)))
                    (setq l-aux (list))
                    (setf posx 0)          
                (incf posy)))))
                resul))
                      
;--------------------------------------------;
; Fun��o que percorre o tabuleiro inicial    ;
; e determina os diferentes blocos existentes;
; -------------------------------------------;
; ARG1 - tabuleiro com as pe�as              ;
; ARG2 - limite esquerda                     ;
; ARG3 - limite direita                      ;
; ARG4 - limite cima                         ;
; ARG5 - limite baixo                        ;
; ARG6 - numero linhas do tabuleiro          ;
; ARG7 - numero colunas do tabuleiro         ;
; ARG8 - hash com bloocs                     ;
;--------------------------------------------;

(defun lista-blocos-estado-inicial (tabuleiro x-ini x-fin y-ini y-fin n-lin n-col hash)
  (let* ((resul hash)
         (contador 0)
         (p-aux (make-peca))
         (b-aux (make-bloco)))
    (print "entrou: lista-blocos-estado-inicial")
    (loop for posy from y-ini to y-fin do
          (loop for posx from x-ini to x-fin do
                (setq p-aux (nth posx (nth posy tabuleiro)))
                (if (= (peca-bloco p-aux) -1)                                                                                                                 ; V� se a pe�a j� est� num bloco                          
                    (progn                                                                                                                                    ; Se n�o estiver num bloco
                      (setf (peca-bloco p-aux) contador)                                                                                                      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                      (setq b-aux (make-bloco :cor (peca-cor p-aux) :lista-pecas (list p-aux) :id contador :x-min posx :x-max posx :y-min posy :y-max posy))  ;; Cria um bloco para a pe�a
                       (setf (gethash (peca-bloco p-aux) resul) b-aux)                                                                                        ;; coloca-a no bloco
                      (setf (nth posx (nth posy tabuleiro)) p-aux)                                                                                            ;; e guarda o bloco para refer�ncia
                      (incf contador))                                                                                                                        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                    (setf b-aux (gethash (peca-bloco p-aux) resul)))     ; Se j� estiver num bloco, guarda o bloco para refer�ncia
                (if (not (>= posx (- n-col 1)))
                    (ve-frente tabuleiro p-aux b-aux posx posy resul))   ; V� se bloco � direita � da mesma cor
                (if (not (>= posy (- n-lin 1)))
                    (ve-abaixo tabuleiro p-aux b-aux posx posy resul)))) ; V� se bloco em baixo � da mesma cor   
    resul))

;----------------------------------------------------------------------;
; Fun��o que verifica se a pe�a � direita pertence ao mesmo bloco      ;
; ---------------------------------------------------------------------;
; ARG1 - tabuleiro com as pe�as                                        ;
; ARG2 - pe�a a partir da qual se verifica se � pertence ao mesmo bloco;
; ARG3 - bloco a adicionar caso seja da mesma cor                      ;
; ARG4 - coord.x da posicao da pe�a p-aux                              ;
; ARG5 - coord.y da posicao da pe�a p-aux                              ;
; ARG6 - hash com blocos                                               ;
;----------------------------------------------------------------------;

(defun ve-frente (tabuleiro p-aux b-aux posx posy hash)
  (let* ((l-aux (bloco-lista-pecas b-aux))
         (p-dir (nth (+ posx 1) (nth posy tabuleiro)))
         (chave-1 (peca-bloco p-aux))
         (chave-2 (peca-bloco p-dir)))
    (print "entrou: ve-frente")
    (if (= (peca-cor p-aux) (peca-cor p-dir))                                         ; Se o da frente for igual
        (if (= -1 chave-2)
            (progn
              (format t "Right Match On: posx: ~D posy: ~D ~% " posx posy)
              (setf (peca-bloco p-dir) (peca-bloco p-aux))                            ; Junta a informa��o do bloco � pe�a da direita
              (setf (nth (+ posx 1) (nth posy tabuleiro)) p-dir)                      ; Coloca a pe�a atualizada no tabuleiro
              (setq l-aux (append l-aux (list p-dir)))                                ; Adiciona a pe�a � lista para atualizar o bloco
              (setf (bloco-lista-pecas (gethash (peca-bloco p-dir) hash)) l-aux)      ; Atualiza o bloco na hash
              (setf (bloco-x-max (gethash (peca-bloco p-dir) hash)) (+ posx 1)))      ; Incrementa O xmax do bloco     
          (if (not (= chave-1 chave-2))
              (junta-blocos tabuleiro hash chave-1 chave-2))))))


;----------------------------------------------------------------------;
; Fun��o que verifica se a pe�a abaixo pertence ao mesmo bloco         ;
; ---------------------------------------------------------------------;
; ARG1 - tabuleiro com as pe�as                                        ;
; ARG2 - pe�a a partir da qual se verifica se � pertence ao mesmo bloco;
; ARG3 - bloco a adicionar caso seja da mesma cor                      ;
; ARG4 - coord.x da posicao da pe�a p-aux                              ;
; ARG5 - coord.y da posicao da pe�a p-aux                              ;
; ARG6 - hash com blocos                                               ;
;----------------------------------------------------------------------;

(defun ve-abaixo (tabuleiro p-aux b-aux posx posy hash)
  (let* ((l-aux (bloco-lista-pecas b-aux))
         (p-baixo (nth posx (nth (+ posy 1) tabuleiro)))
         (chave-1 (peca-bloco p-aux))
         (chave-2 (peca-bloco p-baixo)))
    (print "entrou: ve-abaixo")
    (if (= (peca-cor p-aux) (peca-cor p-baixo))                                             ; Se o da frente for igual
        (if (= -1 chave-2)
                (progn
                  (format t "Down Match On: posx: ~D posy: ~D ~% " posx posy)              
                  (setf (peca-bloco p-baixo) (peca-bloco p-aux))                            ; Junta a informa��o do bloco � pe�a da direita
                  (setf (nth posx (nth (+ posy 1) tabuleiro)) p-baixo)                      ; Coloca a pe�a atualizada no tabuleiro
                  (setq l-aux (append l-aux (list p-baixo)))                                ; Adiciona a pe�a � lista para atualizar o bloco
                  (setf (bloco-lista-pecas (gethash (peca-bloco p-baixo) hash)) l-aux)      ; Atualiza o bloco na hash
                  (setf (bloco-y-max (gethash (peca-bloco p-baixo) hash)) (+ posy 1)))      ; Incrementa o ymax do bloco 
          (if (not (= chave-1 chave-2))        
              (junta-blocos tabuleiro hash chave-1 chave-2))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;	FUN��O PRINCIPAL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

(defun resolve-same-game (problema algoritmo)
  (let* ((tab (cria-tabuleiro problema (list-length (first problema))))
         (h-blocos (lista-blocos-estado-inicial tab 0 (- (list-length (first problema)) 1) 0 (- (list-length problema) 1) (list-length problema) (list-length (first problema)) (make-hash-table)))
         (estado-inicial (make-no :n-pecas (* (list-length problema) (list-length (first problema))) :n-blocos (hash-table-count h-blocos) :tabuleiro tab :h-blocos h-blocos :n-linhas (list-length problema) :n-colunas (list-length (first problema))))
        ; (gera-sucessores	#'sucessores)
        ; (heuristica1		#'heur-melhor-primeiro)
        ; (heuristica2		#'heur-melhor-primeiro-posicao-menor)
        ; (heuristica-opt	#'heur-menor-altura)
         resul solucao)
    (print (print-tabuleiro tab (- (list-length problema) 1) (- (list-length (first problema)) 1)))
    (print-hash h-blocos)
    (print (gethash 3 h-blocos))
    estado-inicial)
)


(print (resolve-same-game '((1 1 1 10 8) (1 2 2 1 3) (1 2 2 1 2) (1 1 1 1 1))
 "melhor.abordagem.optimizacao"))


