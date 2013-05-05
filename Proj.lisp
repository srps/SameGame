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
;;	DEFINICOES  DE CONSTANTES 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defconstant MAX-TEMPO 240) ;;Tempo limite de tempo para execu��o



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;	TEMPO
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;--------------------------------------------------------------------------;
; Fun��o que calcula tempo restante                                        ;
;--------------------------------------------------------------------------;
; ARG1 - tempo inicia                                                      ;
; ARG2 - segundos passados                                                 ;
;--------------------------------------------------------------------------;             
(defun time-to-stop? (tempo-inicio n-segundos)
        (<= (* n-segundos INTERNAL-TIME-UNITS-PER-SECOND) (- (get-start-time) tempo-inicio)))

;--------------------------------------------------------------------------;
; Fun��o que retorna o tempo interno actual                                ;
;--------------------------------------------------------------------------;
; ARG1 - tempo inicia                                                      ;
; ARG2 - segundos passados                                                 ;
;--------------------------------------------------------------------------; 
(defun get-start-time ()
                (get-internal-run-time))




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
  maior-bloco                 ; Tamanho do maior bloco
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                               
;;   FUNCAO ESTADO-OBJECTIVO   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                              
(defun objectivo? (estado)
        (eq 0 (no-n-pecas estado)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;	DEBUG
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun print-tabuleiro (tabuleiro n-lin n-col)
  (let* ((result '())
         (temp-list '()))
    (loop for posy from 0 to n-lin do
          (loop for posx from 0 to n-col do
                (if (not (eq nil (nth posx (nth posy tabuleiro))))
                (setq temp-list (append temp-list (list (peca-cor (nth posx (nth posy tabuleiro))))))
                 (setq temp-list (append temp-list (list -1)))) ; Caso N�o exista pe�a coloca -1
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
       (format t "~% Key: ~D Cor Bloco: ~D Numero De Pe�as: ~D Xmin: ~D Xmax: ~D Ymin: ~D Ymax: ~D Lista Pe�as: ~A" key (bloco-cor b-aux) (list-length (bloco-lista-pecas b-aux)) (bloco-x-min b-aux) (bloco-x-max b-aux) (bloco-y-min b-aux) (bloco-y-max b-aux) (bloco-lista-pecas b-aux))
       )
 )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;	SUCESSORES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;--------------------------------------------------------------------------;
; Fun��o que gera sucessores                                               ;
;--------------------------------------------------------------------------;
; ARG1 - estado                                                            ;
;--------------------------------------------------------------------------;

(defun gera-sucessores (estado)
  (let* ((tabuleiro (no-tabuleiro estado))
         (hash (no-h-blocos estado))
         (nr-linhas (no-n-linhas estado))
         (nr-colunas (no-n-colunas estado))
         (lista()))
    (loop for key being the hash-keys of hash do     
          (let* ((novo-estado (copia-estado estado))
                 (b-aux (gethash key (no-h-blocos novo-estado))))
            (print novo-estado)
             (print (print-tabuleiro (no-tabuleiro novo-estado) 4 4))
            (remove-bloco novo-estado key (no-h-blocos novo-estado))
            (gravidade (no-tabuleiro novo-estado) b-aux (no-h-blocos novo-estado))
            (encosta-esquerda novo-estado (no-tabuleiro novo-estado) (no-h-blocos novo-estado))
            (print "--------------------------")
            (print (no-n-colunas novo-estado))
            (print (no-n-linhas novo-estado))
            (setf (no-h-blocos novo-estado) (lista-blocos (no-tabuleiro novo-estado) 0 (- (no-n-colunas novo-estado) 1) 0 (- (no-n-linhas novo-estado) 1) (no-n-linhas novo-estado) (no-n-colunas novo-estado) (no-h-blocos novo-estado)))
            (maior-bloco novo-estado (no-h-blocos novo-estado))
            (print "HASH ANTIGA")
            (print (print-hash hash))
            (print "HASH NOVA")
            (print (print-hash (no-h-blocos novo-estado)))
            ))
    ))
  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;	HEURISTICAS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;	PROCURA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun procura (estado sucessores heuristica)
  (funcall #'gera-sucessores estado)
  )


(defun procura-alternativa (estado sucessores heuristica)
)

(defun sondagem-iterativa(estado-inicial)
  (let* (	
         (melhor-resultado 0)
         ;(melhor-resultado (list (list (make-estado	:cantos nil :colocadas nil :livres nil
	;									:rectangulo (list +MAIOR_RAIO+ +MAIOR_RAIO+))) 0 0 0))
		;	(tentativas 0)
		;	(melhor-altura +BIG_INT+)
		;	(melhor-tentativa)
		;	resultado
         (ngerados 0)
         (nexpandidos 0)
         (max-segundos 5)	; 4 mins
         (tempo-inicio (get-internal-run-time)) 
							; (inicio + Nsegs*Tickspsec)
         (tempo-limite (+ tempo-inicio (* max-segundos internal-time-units-per-second)))
		;	(tempo-melhor tempo-inicio)
         )
		 
    (print ngerados)
    (loop while (> tempo-limite (get-internal-run-time)) do
		;	(if (car (setf resultado (chama-procura estado-inicial #'sucessores-sondagem nil "profundidade")))
		;		(let ((altura (maior-altura (estado-colocadas (resultado-solucao resultado)))))
		;			(if (< altura melhor-altura)
		;				(progn	; Optimizacao: corre a proxima ronda com a nova altura minima encontrada
		;						(setf melhor-altura	(setf (second (estado-rectangulo estado-inicial)) altura))
		;						(setf tempo-melhor (get-internal-run-time))
		;						(setf melhor-resultado	resultado)
		;						(setf melhor-tentativa tentativas)
		;						(printf "Menor Altura ja' encontrada: %d" melhor-altura)
		;				))))
		;	
		;	(incf tentativas)
		;	(incf nexpandidos	(caddr resultado))
		;	(incf ngerados		(cadddr resultado)))

		;(decf tempo-melhor tempo-inicio)
		
		;(setf (cadr		melhor-resultado) (- (get-internal-run-time) tempo-inicio))
		;(setf (caddr	melhor-resultado) nexpandidos)
		;(setf (cadddr	melhor-resultado) ngerados)
          ;(printf "Max ticks: %d, Tentativas: %d" (cadr melhor-resultado) tentativas)
          ;(printf "Melhor solucao na %d tentativa, com %d ticks" melhor-tentativa tempo-melhor)
          )
          melhor-resultado)
    )





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;	FUN��ES AUXILIARES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun copy-hash (hash)
  (let* ((new-hash (make-hash-table)))
    (loop for key being the hash-keys of hash do     
          (setf (gethash key new-hash) (gethash key hash)))
    new-hash) 
)
  


;--------------------------------------------------------------------------;
; Fun��o que devolve a maior chave da hash table                           ;
;--------------------------------------------------------------------------;
; ARG1 - Hash Table                                                            ;
;--------------------------------------------------------------------------;

(defun ve-maior-hash (ht)
  (let* ((resul -1))
    (loop for k being the hash-keys of ht do
          (when (> k resul) (setq resul k)))
    resul))


;--------------------------------------------------------------------------;
; Fun��o que efectua a c�pia de um estado                                  ;
;--------------------------------------------------------------------------;
; ARG1 - estado                                                            ;
;--------------------------------------------------------------------------;


(defun copia-estado (estado)
  (make-no        :tabuleiro (copy-list (no-tabuleiro estado))
                  :h-blocos (copy-hash (no-h-blocos estado))
                  :pontuacao (no-pontuacao estado)
                  :n-pecas (no-n-pecas estado)
                  :n-blocos (no-n-blocos estado)
                  :n-linhas (no-n-linhas estado)
                  :n-colunas (no-n-colunas estado)
                  :maior-bloco 0))
                                                                      



;--------------------------------------------------------------------------;
; Fun��o que devolve o tamanho do maior bloco da hash                      ;
;--------------------------------------------------------------------------;
; ARG1 - estado                                                            ;
; ARG2 - hash                                                              ;
;--------------------------------------------------------------------------;

(defun maior-bloco (estado hash)
  (let* ((result 0)
         (tmp 0)
          (b-aux (make-bloco)))
          (loop for key being the hash-keys of hash do
                (setf b-aux (gethash key hash))
                (if (< result (list-length (bloco-lista-pecas b-aux)))
                    (setf result (list-length (bloco-lista-pecas b-aux)))))
          (setf (no-maior-bloco estado) result)   
))

;--------------------------------------------------------------------------;
; Fun��o que remove bloco do tabuleiro e hash table e atualiza a pontua��o ;
;--------------------------------------------------------------------------;
; ARG1 - Estado (n�)                                                       ;
; ARG2 - ID do bloco a ser removido                                        ;
; ARG3 - Hash table                                                        ;
;--------------------------------------------------------------------------;

(defun remove-bloco (estado id-bloco ht)
  (let* ((l-aux (bloco-lista-pecas (gethash id-bloco ht)))
         (pontos (expt (- (list-length l-aux) 2) 2))
         (pos))
    (remhash id-bloco ht)
    (setf (no-pontuacao estado) (+ (no-pontuacao estado) pontos))
    (setf (no-n-pecas estado) (- (no-n-pecas estado) (list-length l-aux)))
    (loop for p-aux in l-aux do
          (setq pos (peca-pos p-aux))
          (setf (nth (car pos) (nth (cdr pos) (no-tabuleiro estado))) NIL))))

;--------------------------------------------------------------------------;
; Fun��o que faz cair as pe�as consoante as leis da gravidade              ;
;--------------------------------------------------------------------------;
; ARG1 - Tabuleiro do jogo                                                 ;
; ARG2 - Bloco que foi removido (fornece as coordenadas)                   ;
; ARG3 - Hash table                                                        ;
;--------------------------------------------------------------------------;

(defun gravidade (tabuleiro bloco ht)
  (let* ((x-ini (bloco-x-min bloco))
         (x-fin (bloco-x-max bloco))
         (y-ini (bloco-y-max bloco))
         (p-aux)
         (contador 0))
    (print "entrou: gravidade")
    (loop for coluna from x-ini to x-fin do
          (loop for linha from y-ini downto 0 do
                (setq p-aux (nth coluna (nth linha tabuleiro)))
                (if (not (eq p-aux NIL))                                                     ; Se houver pe�a na posi��o indicada
                    (if (> contador 0)                                                       ; Se houver espa�os vazios abaixo da pe�a
                        (progn
                          (remhash (peca-bloco p-aux) ht)                                      ; --Remove o bloco da hash
                          (setf (cdr (peca-pos p-aux)) (+ (cdr (peca-pos p-aux)) contador))  ; Puxa a pe�a para baixo
                          (setf (peca-bloco p-aux) -1)                                       ; Remove o bloco da pe�a
                          (setf (nth coluna (nth linha tabuleiro)) NIL)                      ; Atualiza o tabuleiro
                          (setf (nth coluna (nth (+ linha contador) tabuleiro)) p-aux)))     ; Atualiza o tabuleiro
                  (incf contador)))                                                          ; Se for uma posi��o vazia, incrementa o contador
          (setq contador 0))))                                                               ; Reset do contador a cada coluna nova


;---------------------------------------------------------------------------;
; Fun��o que encosta as pe�as � esquerda, eliminando colunas vazias no meio ;
;---------------------------------------------------------------------------;
; ARG1 - estado
; ARG2 - Tabuleiro do jogo                                                  ;
; ARG3 - Hash table                                                         ;
;---------------------------------------------------------------------------;
; RET  - N� de shifts que efetuou                                           ;
;---------------------------------------------------------------------------;


(defun encosta-esquerda (estado tabuleiro ht)
  (let* ((x-fin (- (list-length (first tabuleiro)) 1))
         (y-ini (- (list-length tabuleiro) 1))
         (p-aux)
         (contador 0))
    (print "entrou: encosta-esquerda")
    (loop for coluna from 0 to x-fin do
          (if (eq (nth coluna (nth y-ini tabuleiro)) NIL)
              (incf contador)                                                                         ; Se for uma posi��o vazia, incrementa o contador
            (if (> contador 0)
                (loop for linha from y-ini downto 0 do
                      (setq p-aux (nth coluna (nth linha tabuleiro)))
                      (if (not (eq p-aux NIL))                                                        ; Se houver pe�a na posi��o indicada
                          (progn
                            (setf (car (peca-pos p-aux)) (- (car (peca-pos p-aux)) contador))         ; Puxa a pe�a para a esquerda
                            (setf (nth coluna (nth linha tabuleiro)) NIL)                             ; Atualiza o tabuleiro
                            (setf (nth (- coluna contador) (nth linha tabuleiro)) p-aux))             ; Atualiza o tabuleiro
                        (return))))))                                                                 ; Quando v� NIL, salta para a pr�xima coluna
    (print contador)
    (setf (no-n-colunas estado) (- (no-n-colunas estado) contador))
    contador))      



;----------------------------------------;
; Fun��o que junta 2 blocos da mesma cor ;
; Mant�m o 1� bloco que � passado        ;
; ---------------------------------------;
; ARG1 - tabuleiro com as pe�as          ;
; ARG2 - hashtable dos blocos            ;
; ARG3 - chave do bloco a manter         ;
; ARG4 - chave do bloco que desaparece   ;
;----------------------------------------;

(defun junta-blocos (tabuleiro ht chave-b1 chave-b2)
  (let* ((b-aux (gethash chave-b1 ht))                                   ; Refer�ncia para o bloco que se vai manter
         (b-trash (gethash chave-b2 ht))                                 ; Refer�ncia para o bloco que vai � vida
         (l-aux (bloco-lista-pecas b-aux))                               ; Lista das pe�as do bloco que se vai manter
         (posx)
         (posy)
         (xmin (bloco-x-min b-trash))                                    ; X M�nimo do bloco que vai � vida
         (xmax (bloco-x-max b-trash))                                    ; X M�ximo do bloco que vai � vida
         (ymin (bloco-y-min b-trash))                                    ; Y M�nimo do bloco que vai � vida
         (ymax (bloco-y-max b-trash)))                                   ; Y M�ximo do bloco que vai � vida
  (loop for p-aux in (bloco-lista-pecas b-trash) do
        (setq posx (car (peca-pos p-aux)))                               ; Guarda a coordenada x da pe�a a ser mudada
        (setq posy (cdr (peca-pos p-aux)))                               ; Guarda a coordenada y da pe�a a ser mudada
        (setf (peca-bloco p-aux) chave-b1)                               ; Muda o bloco da pe�a
        (setq l-aux (append l-aux (list p-aux)))                         ; Insere a pe�a na lista do bloco original
        (setf (nth posx (nth posy tabuleiro)) p-aux))                    ; Re-insere a pe�a no tabuleiro
  (setf (bloco-lista-pecas b-aux) l-aux)                                 ; Coloca a nova lista no bloco original
  (if (> (bloco-x-min b-aux) xmin)                                       ;;---------------------------------------------;
      (setf (bloco-x-min b-aux) xmin))                                   ;;                                             ;
  (if (< (bloco-x-max b-aux) xmax)                                       ;;                                             ;
      (setf (bloco-x-max b-aux) xmax))                                   ;; Atualiza os m�nimos e m�ximos do bloco      ;
  (if (> (bloco-y-min b-aux) ymin)                                       ;;                                             ;
      (setf (bloco-y-min b-aux) ymin))                                   ;;                                             ;
  (if (< (bloco-y-max b-aux) ymax)                                       ;;                                             ;
      (setf (bloco-y-max b-aux) ymax))                                   ;;---------------------------------------------;
  (setf (gethash chave-b1 ht) b-aux)                                     ; Atualiza o bloco original na HT
  (remhash chave-b2 ht)))                                                ; Remove o 2� bloco da HT



;--------------------------------------------------------;
; Fun��o que actualiza as margens do bloco ap�s jun��o   ;
; -------------------------------------------------------;
; ARG1 - bloco a actualizar                              ;
;--------------------------------------------------------;

(defun actualiza-margens (b-aux)
  (let* ((xmin 0)
         (xmax 0)
         (ymin 0)
         (ymax 0)
         (pecas (bloco-lista-pecas b-aux)))
    (setf xmin (car (peca-pos (car pecas))))
    (setf ymin (cdr (peca-pos (car pecas))))
    (loop for p-aux in (cdr pecas) do
          (print p-aux)
          (if (< (car (peca-pos p-aux)) xmin)
              (setf xmin  (car (peca-pos p-aux))))
          (if (> (car (peca-pos p-aux)) xmax)
              (setf xmax  (car (peca-pos p-aux))))
          (if (< (cdr (peca-pos p-aux)) ymin)
              (setf ymin  (cdr (peca-pos p-aux))))
          (if (> (cdr (peca-pos p-aux)) ymax)
              (setf ymax  (cdr (peca-pos p-aux)))))
    (setf (bloco-x-min b-aux) xmin)
    (setf (bloco-x-max b-aux) xmax)
    (setf (bloco-y-min b-aux) ymin)
    (setf (bloco-y-max b-aux) ymax)
    b-aux))


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
         (p-aux)
         (b-aux)
         (contador (+ (ve-maior-hash hash) 1)))
    (print "entrou: lista-blocos")
    (loop for posy from y-ini to y-fin do
          (loop for posx from x-ini to x-fin do
                (if (not (eq (nth posx (nth posy tabuleiro)) nil))
                    (progn      
                      (setq p-aux (nth posx (nth posy tabuleiro)))
                      (if (= (peca-bloco p-aux) -1)                                                                                                                 ; V� se a pe�a j� est� num bloco                          
                          (progn                                                                                                                                    ; Se n�o estiver num bloco
                            (setf (peca-bloco p-aux) contador)                                                                                                      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                            (setq b-aux (make-bloco :cor (peca-cor p-aux) :lista-pecas (list p-aux) :id contador :x-min posx :x-max posx :y-min posy :y-max posy))  ;; Cria um bloco para a pe�a
                            (setf (gethash (peca-bloco p-aux) resul) b-aux)                                                                                         ;; coloca-a no bloco
                            (setf (nth posx (nth posy tabuleiro)) p-aux)                                                                                            ;; e guarda o bloco para refer�ncia
                            (incf contador))
                        (setf b-aux (gethash (peca-bloco p-aux) resul)))
                      (if (and (not (>= posx (- n-col 1)))                                                          ; Estou na �ltima coluna do tabuleiro?
                               (not (eq (nth (+ posx 1) (nth posy tabuleiro)) nil)))                                ; A pe�a � frente existe?
                          (ve-frente tabuleiro p-aux b-aux posx posy resul))                                        ; --Se n�o, verifica bloco � direita
                      (if (not (>= posy (- n-lin 1)))                                                               ; Estou na �ltima linha do tabuleiro?                                                                                   
                          (ve-abaixo tabuleiro p-aux b-aux posx posy resul))))))                                    ; --Se n�o, verifica bloco em baixo
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

(defun ve-frente (tabuleiro p-aux b-aux posx posy ht)
  (let* ((l-aux (bloco-lista-pecas b-aux))
         (p-dir (nth (+ posx 1) (nth posy tabuleiro)))
         (chave-b1 (peca-bloco p-aux))
         (chave-b2 (peca-bloco p-dir)))
    ;(print "entrou: ve-frente")
        (if (= (peca-cor p-aux) (peca-cor p-dir))                                         ; Se o da frente for igual
            (if (= -1 chave-b2)
                (progn
              ;(format t "Right Match On: posx: ~D posy: ~D ~% " posx posy)
                  (setf (peca-bloco p-dir) (peca-bloco p-aux))                            ; Junta a informa��o do bloco � pe�a da direita
                  (setf (nth (+ posx 1) (nth posy tabuleiro)) p-dir)                      ; Coloca a pe�a atualizada no tabuleiro
                  (setq l-aux (append l-aux (list p-dir)))                                ; Adiciona a pe�a � lista para atualizar o bloco
                  (setf (bloco-lista-pecas (gethash (peca-bloco p-dir) ht)) l-aux)        ; Atualiza o bloco na hash
                  (setf (bloco-x-max (gethash (peca-bloco p-dir) ht)) (+ posx 1)))        ; Incrementa O xmax do bloco     
              (if (not (= chave-b1 chave-b2))
                  (if (>= (list-length (bloco-lista-pecas (gethash chave-b1 ht))) 
                          (list-length (bloco-lista-pecas (gethash chave-b2 ht))))
                      (junta-blocos tabuleiro ht chave-b1 chave-b2)
                    (junta-blocos tabuleiro ht chave-b2 chave-b1)))))))


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

(defun ve-abaixo (tabuleiro p-aux b-aux posx posy ht)
  (let* ((l-aux (bloco-lista-pecas b-aux))
         (p-baixo (nth posx (nth (+ posy 1) tabuleiro)))
         (chave-b1 (peca-bloco p-aux))
         (chave-b2 (peca-bloco p-baixo)))
    ;(print "entrou: ve-abaixo")
    (if (= (peca-cor p-aux) (peca-cor p-baixo))                                             ; Se o da frente for igual
        (if (= -1 chave-b2)
                (progn
                  ;(format t "Down Match On: posx: ~D posy: ~D ~% " posx posy)              
                  (setf (peca-bloco p-baixo) (peca-bloco p-aux))                            ; Junta a informa��o do bloco � pe�a da direita
                  (setf (nth posx (nth (+ posy 1) tabuleiro)) p-baixo)                      ; Coloca a pe�a atualizada no tabuleiro
                  (setq l-aux (append l-aux (list p-baixo)))                                ; Adiciona a pe�a � lista para atualizar o bloco
                  (setf (bloco-lista-pecas (gethash (peca-bloco p-baixo) ht)) l-aux)        ; Atualiza o bloco na hash
                  (setf (bloco-y-max (gethash (peca-bloco p-baixo) ht)) (+ posy 1)))        ; Incrementa o ymax do bloco 
          (if (not (= chave-b1 chave-b2))        
              (if (>= (list-length (bloco-lista-pecas (gethash chave-b1 ht))) 
                      (list-length (bloco-lista-pecas (gethash chave-b2 ht))))
                  (junta-blocos tabuleiro ht chave-b1 chave-b2)
                (junta-blocos tabuleiro ht chave-b2 chave-b1)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;	FUN��O PRINCIPAL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

(defun resolve-same-game (problema algoritmo)
  (let* ((tab (cria-tabuleiro problema (list-length (first problema))))
         (h-blocos (lista-blocos tab 0 (- (list-length (first problema)) 1) 0 (- (list-length problema) 1) (list-length problema) (list-length (first problema)) (make-hash-table)))
         (estado-inicial (make-no :n-pecas (* (list-length problema) (list-length (first problema))) :n-blocos (hash-table-count h-blocos) :tabuleiro tab :h-blocos h-blocos :n-linhas (list-length problema) :n-colunas (list-length (first problema)) :maior-bloco 0))
         ;(b-aux (gethash 0 h-blocos))
        ; (g-sucessores	#'gera-sucessores)
        ; (heuristica1		#'heur-melhor-primeiro)
        ; (heuristica2		#'heur-melhor-primeiro-posicao-menor)
        ; (heuristica-opt	#'heur-menor-altura)
         resul solucao)


    (setf resul
          (cond ((string-equal algoritmo "melhor.abordagem")
                 (procura estado-inicial (list #'gera-sucessores) heuristica1))

                ((string-equal algoritmo "a*.melhor.heuristica")
                 (procura estado-inicial g-sucessores heuristica1))

                ((string-equal algoritmo "a*.melhor.heuristica.alternativa")
                 (procura estado-inicial g-sucessores heuristica2))

                ((string-equal algoritmo "sondagem.iterativa")
                 (sondagem-iterativa estado-inicial))

                ((string-equal algoritmo "abordagem.alternativa")
                 (procura-alternativa estado-inicial gera-sucessores heuristica1))))

    ;(print (print-tabuleiro tab (- (no-n-linhas estado-inicial) 1) (- (no-n-colunas estado-inicial) 1))) 
    ;(print (print-hash h-blocos))
    ;(maior-bloco estado-inicial h-blocos) 
    ;(remove-bloco estado-inicial 0 h-blocos)
    ;(gravidade tab b-aux h-blocos)
    ;(encosta-esquerda estado-inicial tab h-blocos)
    ;(print (print-hash h-blocos))
    ;(print (lista-blocos tab 0 (- (no-n-colunas estado-inicial) 1) 0 (- (no-n-linhas estado-inicial) 1) (no-n-linhas estado-inicial) (no-n-colunas estado-inicial) h-blocos)) 
    ;(print (print-tabuleiro tab (- (no-n-linhas estado-inicial) 1) (- (no-n-colunas estado-inicial) 1))) 
    ;(print (print-hash h-blocos))
    ;(maior-bloco estado-inicial h-blocos)
    (print "FIMMMMMMMMMMMM")
    estado-inicial)
)


;(print (resolve-same-game '((1 1 1 10 8) (1 2 2 1 3) (1 2 2 1 2) (1 1 1 1 1))
; "sondagem.iterativa"))

(print (resolve-same-game '((1 10 10 10 8) (1 2 1 1 1) (1 10 1 10 2) (1 1 1 10 10)) "melhor.abordagem"))
