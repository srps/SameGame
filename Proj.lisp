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



(eval-when (compile) (declaim (optimize (speed 3) (safety 0) (debug 0))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;	DEFINICOES  DE CONSTANTES 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defconstant MAX-TEMPO 250) ;;Tempo limite de tempo para execu��o



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

(defvar *max-result* 0)
(defvar tempo-inicio (get-internal-run-time))
(defvar *tamanho-tabuleiro* 0)

(defstruct peca
  pos                         ; posi��o da pe�a (x . y)
  (cor -1 :type fixnum)       ; cor da pe�a
  (bloco -1 :type fixnum)     ; bloco a que pertence a pe�a
)

(defstruct bloco
  (cor -1 :type fixnum)       ; cor do bloco
  lista-pecas                 ; lista das pe�as do bloco
  (id -1 :type fixnum)        ; identificador do bloco
  (x-min 20 :type fixnum)
  (x-max -1 :type fixnum)
  (y-min 20 :type fixnum)
  (y-max -1 :type fixnum)
)

	
(defstruct nos
  (pontuacao 0 :type fixnum) ; Pontua��o at� ao momento do estado
  (prof 0 :type fixnum)
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
 (time-to-stop? tempo-inicio MAX-TEMPO))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;	DEBUG
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun print-tabuleiro (tabuleiro n-lin n-col)
  (let* ((result '())
         (temp-list '()))
    (loop for posy from 0 to (- n-lin 1) do
          (loop for posx from 0 to (- n-col 1) do
                (if (not (eq nil (nth posx (nth posy tabuleiro))))
                (setq temp-list (append temp-list (list (peca-cor (nth posx (nth posy tabuleiro))))))
                 (setq temp-list (append temp-list (list -1))))) ; Caso N�o exista pe�a coloca -1
          (setq result (append result (list temp-list)))
          (setf temp-list '()))
  (print result)))

(defun print-hash (hash)
  (let* ((b-aux (make-bloco)))
 (loop for key being the hash-keys of hash do
       (setf b-aux (gethash key hash))
       (format t "~% Key: ~D Cor Bloco: ~D Numero De Pe�as: ~D Xmin: ~D Xmax: ~D Ymin: ~D Ymax: ~D Lista Pe�as: ~A" 
               key (bloco-cor b-aux) (list-length (bloco-lista-pecas b-aux)) 
               (bloco-x-min b-aux) (bloco-x-max b-aux) (bloco-y-min b-aux) 
               (bloco-y-max b-aux) (bloco-lista-pecas b-aux)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;	SUCESSORES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;--------------------------------------------------------------------------;
; Fun��o que gera sucessores                                               ;
;--------------------------------------------------------------------------;
; ARG1 - estado                                                            ;
;--------------------------------------------------------------------------;
; RET  - Devolve a lista com os estados sucessores                         ;
;--------------------------------------------------------------------------;

(defun gera-sucessores (estado)
  (let* ((tabuleiro (nos-tabuleiro estado))
         (hash (nos-h-blocos estado))
         (nr-linhas (nos-n-linhas estado))
         (nr-colunas (nos-n-colunas estado))
         (lista()))
    ;(print "SUCESSORES")
    ;(print (print-hash hash))
    (loop for key being the hash-keys of hash do     
          (let* ((novo-estado (copia-estado estado))
                 (b-aux (gethash key (nos-h-blocos novo-estado))))
            (if (>= (list-length (bloco-lista-pecas b-aux)) 2)
                (progn
                  (atualiza-tabuleiro (nos-tabuleiro novo-estado) (nos-h-blocos novo-estado))
                  (remove-bloco novo-estado key (nos-h-blocos novo-estado))
                  (let* ((l-margens
                          (gravidade (nos-tabuleiro novo-estado) b-aux (nos-h-blocos novo-estado)))
                         (l-m2 
                          (encosta-esquerda novo-estado (nos-tabuleiro novo-estado) (nos-h-blocos novo-estado) l-margens)))
                    (setf (nos-h-blocos novo-estado) (lista-blocos (nos-tabuleiro novo-estado) 
                                                                  (first l-m2) (second l-m2) 
                                                                  0 (third l-m2) 
                                                                  (nos-n-linhas novo-estado) (nos-n-colunas novo-estado) (nos-h-blocos novo-estado))))
                  (maior-bloco novo-estado (nos-h-blocos novo-estado))
                  (incf (nos-prof novo-estado))
                  (push novo-estado lista)
            ))))
    lista))
  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;	HEURISTICAS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;--------------------------------------------------------------------------;
; Heuristica  que d� mais prioridade a n�s com menos blocos                ;
;--------------------------------------------------------------------------;
; ARG1 - estado                                                            ;
;--------------------------------------------------------------------------;


(defun heuristica1 (estado)
  (- *tamanho-tabuleiro* (nos-maior-bloco estado)))


;Heuristica que adapta a heuristica 1 
;para conseguir funcionar com a*
;d� mais import�ncia aos estados que est�o a maior profundidade         
(defun heuristica2 (estado)
  (+ (* 144 (- 72 (nos-prof estado)))
     (heuristica1 estado)))

(defun heuristica3 (estado)
  (+ (* 144 (- 72 (nos-prof estado)))
     (nos-pontuacao estado)))

(defun heuristica4 (estado)
  (+ (heuristica1 estado) (nos-pontuacao estado)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;	PROCURA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun procura-tabuleiro (estado sucessores heuristica)
  ;(print (funcall #'gera-sucessores estado))
  )



(defun procura-alternativa (estado sucessores heuristica))






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;	FUN��ES AUXILIARES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun converte-solucao  (solucao)
  (let* ((resultado-temp (first solucao))
         (resultado))
    (if (null resultado-temp)
        nil
      (progn
        (loop for no-a-converter in resultado-temp do
             (print (print-hash (nos-h-blocos no-a-converter))))))
        resultado
  ))



(defun atualiza-tabuleiro (tabuleiro ht)
  (let* ((pos))
    ;(print "entrou: atualiza-tabuleiro")
    (loop for bl being the hash-values of ht do
          (loop for p-aux in (bloco-lista-pecas bl) do
                (setq pos (peca-pos p-aux))
                (setf (nth (car pos) (nth (cdr pos) tabuleiro)) p-aux)
                ;(print "----------------------------------------------------------------------------")
                ;(print tabuleiro)
                ))))

;--------------------------------------------------------------------------;
; Fun��o que cria uma hash table nova a partir de uma existente            ;
;--------------------------------------------------------------------------;
; ARG1 - Hash Table                                                        ;
;--------------------------------------------------------------------------;

(defun copia-hash (hash)
  (let* ((new-hash (make-hash-table))
         (b-aux (make-bloco))
         (l-aux (list))
         (p-aux)
         (novo-bloco))
    (loop for key being the hash-keys of hash do     
          (setq b-aux (gethash key hash))
          (loop for p-ant in (bloco-lista-pecas b-aux) do
                (setq p-aux (make-peca :pos (cons (car (peca-pos p-ant)) (cdr (peca-pos p-ant)))
                                       :cor (peca-cor p-ant)
                                       :bloco (peca-bloco p-ant)))
                (setf l-aux (append l-aux (list p-aux))))
          (setf novo-bloco (make-bloco :cor (bloco-cor b-aux) 
                                       :lista-pecas l-aux 
                                       :id (bloco-id b-aux) 
                                       :x-min (bloco-x-min b-aux) 
                                       :x-max (bloco-x-max b-aux)
                                       :y-min (bloco-y-min b-aux)
                                       :y-max (bloco-y-max b-aux)))
          (setf (gethash key new-hash) novo-bloco)
          (setf l-aux (list)))
    
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
  (make-nos        :tabuleiro (cria-tabuleiro-novo (nos-n-linhas estado) (nos-n-colunas estado))
            :h-blocos (copia-hash (nos-h-blocos estado))
            :prof (nos-prof estado)
                  :pontuacao (nos-pontuacao estado)
                  :n-pecas (nos-n-pecas estado)
                  :n-blocos (nos-n-blocos estado)
                  :n-linhas (nos-n-linhas estado)
                  :n-colunas (nos-n-colunas estado)
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
          (setf (nos-maior-bloco estado) result)   
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
    (setf (nos-pontuacao estado) (+ (nos-pontuacao estado) pontos))
    (setf (nos-n-pecas estado) (- (nos-n-pecas estado) (list-length l-aux)))
    (loop for p-aux in l-aux do
          (setq pos (peca-pos p-aux))
          (setf (nth (car pos) (nth (cdr pos) (nos-tabuleiro estado))) NIL))
        (if (> (nos-pontuacao estado) *max-result*)
        (setf *max-result* (nos-pontuacao estado)) 
      )
  ))

;--------------------------------------------------------------------------;
; Fun��o que faz cair as pe�as consoante as leis da gravidade              ;
;--------------------------------------------------------------------------;
; ARG1 - Tabuleiro do jogo                                                 ;
; ARG2 - Bloco que foi removido (fornece as coordenadas)                   ;
; ARG3 - Hash table                                                        ;
;--------------------------------------------------------------------------;
; RET  - Lista com x-min, x-max e y-max dos blocos que removeu             ;  
;--------------------------------------------------------------------------;

(defun gravidade (tabuleiro bloco ht)
  (let* ((x-ini (bloco-x-min bloco))
         (x-fin (bloco-x-max bloco))
         (y-ini (bloco-y-max bloco))
         (p-aux)
         (bl-aux)
         (b-aux (make-bloco))
         (contador 0)
         (resul (list x-ini x-fin y-ini)))                                                   ; Para evitar ver pe�as desnecess�rias no lista-blocos
    ;(print "entrou: gravidade")                                                              ; --Y min n�o interessa porque as pe�as caem 
    (loop for coluna from 0 to (- (list-length (first tabuleiro)) 1) do
          (loop for linha from (- (list-length tabuleiro) 1) downto 0 do
                (setq p-aux (nth coluna (nth linha tabuleiro)))
                (if (not (eq p-aux NIL))                                                     ; Se houver pe�a na posi��o indicada
                    (if (> contador 0)                                                       ; Se houver espa�os vazios abaixo da pe�a
                        (progn
                          (setf bl-aux (peca-bloco p-aux))
                          (setf b-aux (gethash bl-aux ht))
                          (if (not (eq b-aux NIL))
                              (progn
                                (if (< (bloco-x-min b-aux) (first resul))
                                    (if (= (bloco-x-min b-aux) 0)
                                        (setf (first resul) (bloco-x-min b-aux))
                                      (setf (first resul) (- (bloco-x-min b-aux) 1))))
                                (if (> (bloco-x-max b-aux) (second resul))
                                    (setf (second resul) (bloco-x-max b-aux)))    
                                (if (> (bloco-y-max b-aux) (third resul))
                                    (setf (third resul) (bloco-y-max b-aux)))
                                (loop for p in (bloco-lista-pecas b-aux) do
                                      (setf (peca-bloco p) -1)
                                      (setf (nth (car (peca-pos p)) (nth (cdr (peca-pos p)) tabuleiro)) p))))                            ; Remove o bloco das pe�as)
                          (remhash bl-aux ht)                                                ; --Remove o bloco da hash
                          (setf (cdr (peca-pos p-aux)) (+ (cdr (peca-pos p-aux)) contador))  ; Puxa a pe�a para baixo
                          (setf (nth coluna (nth linha tabuleiro)) NIL)                      ; Atualiza o tabuleiro
                          (setf (nth coluna (nth (+ linha contador) tabuleiro)) p-aux)))     ; Atualiza o tabuleiro
                  (incf contador)))                                                          ; Se for uma posi��o vazia, incrementa o contador
          (setq contador 0))                                                                 ; Reset do contador a cada coluna nova
    resul))                                                               


;---------------------------------------------------------------------------;
; Fun��o que encosta as pe�as � esquerda, eliminando colunas vazias no meio ;
;---------------------------------------------------------------------------;
; ARG1 - estado
; ARG2 - Tabuleiro do jogo                                                  ;
; ARG3 - Hash table                                                         ;
;---------------------------------------------------------------------------;
; RET  - N� de shifts que efetuou                                           ;
;---------------------------------------------------------------------------;


(defun encosta-esquerda (estado tabuleiro ht l-margens)
  (let* ((x-fin (- (list-length (first tabuleiro)) 1))
         (y-ini (- (list-length tabuleiro) 1))
         (p-aux)
         (resul (list (first l-margens) (second l-margens) (third l-margens)))
         (contador 0))
    ;(print "entrou: encosta-esquerda")
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
                            (setf (nth (- coluna contador) (nth linha tabuleiro)) p-aux)              ; Atualiza o tabuleiro
                            (if (not (= (peca-bloco p-aux) -1))
                                (progn
                                  (let* ((b-aux (gethash (peca-bloco p-aux) ht)))
                                    (if (< (- coluna contador) (bloco-x-min b-aux))
                                        (progn
                                          (setf (bloco-x-min (gethash (peca-bloco p-aux) ht)) (- coluna contador))
                                          (if (< (- coluna (+ contador 1)) (first l-margens))
                                              (if (< (- coluna (+ contador 2)) 0)
                                                  (setf (first resul) 0)
                                                (setf (first resul) (- coluna (+ contador 1)))))))                                  
                                    (if (>= coluna (bloco-x-max b-aux))
                                        (setf (bloco-x-max (gethash (peca-bloco p-aux) ht)) (- coluna contador)))))))                       
                        (return))))))                                                                 ; Quando v� NIL, salta para a pr�xima coluna
    (setf (nos-n-colunas estado) (- (nos-n-colunas estado) contador))
    ;(print tabuleiro)
    ;(print contador)
    resul))      



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
    ;(print "entrou: lista-blocos")
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
                            (ve-abaixo tabuleiro p-aux b-aux posx posy resul))))))
    resul))
 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;	FUN��ES DE LEITURA DE TABULEIRO INICIAL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;        


;--------------------------------------------------------------------;
; Fun��o que gera pe�as e constr�i um tabuleiro novo                 ;
; -------------------------------------------------------------------;
; ARG1 - numero de linhas                                            ;
; ARG2 - numero de colunas                                           ;
;--------------------------------------------------------------------;

(defun cria-tabuleiro-novo (n-lin n-col)
  (let* ((resul (list))
         (posx 0)
         (posy 0)
         (l-aux (list)))
   ;(print "entrou: cria-tabuleiro-novo")
    (loop for linha from 0 to (- n-lin 1) do
          (loop for coluna from 0 to (- n-col 1) do
                (setq l-aux (append l-aux (list NIL))))
          (setq resul (append resul (list l-aux)))
          (setq l-aux (list)))
    resul))


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
    ;(print "entrou: cria-tabuleiro")
    (loop for linha in tabuleiro do
          (loop for coluna in linha do
                (if (= coluna -1)
                    (setq p-aux nil)
                  (setq p-aux (make-peca :pos (cons posx posy) :cor coluna :bloco -1)))
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
         (estado-inicial (make-nos :n-pecas (* (list-length problema) (list-length (first problema))) :n-blocos (hash-table-count h-blocos) :tabuleiro tab :h-blocos h-blocos :n-linhas (list-length problema) :n-colunas (list-length (first problema)) :maior-bloco 0))
         ;(b-aux (gethash 0 h-blocos))
        ; (g-sucessores	#'gera-sucessores)
        ; (heuristica1		#'heur-melhor-primeiro)
        ; (heuristica2		#'heur-melhor-primeiro-posicao-menor)
        ; (heuristica-opt	#'heur-menor-altura)
         resul solucao)
    (setf *tamanho-tabuleiro* (* (list-length (nos-tabuleiro estado-inicial)) (list-length (first (nos-tabuleiro estado-inicial)))))
    (print *tamanho-tabuleiro*)

    ;(print (print-hash h-blocos))
    (setf tempo-inicio (get-internal-run-time))
    (setf resul
          (cond ((string-equal algoritmo "melhor.abordagem")
                 (procura-tabuleiro estado-inicial (list #'gera-sucessores) heuristica1))

                ((string-equal algoritmo "a*.melhor.heuristica")
                 (setf solucao (procura (cria-problema estado-inicial (list #'gera-sucessores) :objectivo? #'objectivo? :custo (always 0) :heuristica #'heuristica2) "a*" :espaco-em-arvore? T)))

                ((string-equal algoritmo "a*.melhor.heuristica.alternativa")
                 (procura-tabuleiro estado-inicial g-sucessores heuristica2))

                ((string-equal algoritmo "sondagem.iterativa")
                 (sondagem-iterativa estado-inicial))

                ((string-equal algoritmo "abordagem.alternativa")
                 (setf solucao (time (procura (cria-problema estado-inicial (list #'gera-sucessores) :objectivo? #'objectivo? :estado= #'equal) 
									"profundidade" :espaco-em-arvore? T))))                 ))
    (print "FIM")
    (print *max-result*)
    (setf *max-result* 0)
    (setf tamanho-tabuleiro 0)
    ;(setf solucao (converte-solucao solucao))

  
    solucao))


;(print (resolve-same-game '((1 1 1 10 8) (1 2 2 1 3) (1 2 2 1 2) (1 1 1 1 1))
; "sondagem.iterativa"))

;(print (resolve-same-game '((1 10 10 10 8) (1 2 1 1 1) (1 10 1 10 2) (1 1 1 10 10)) "a*.melhor.heuristica"))

(print (resolve-same-game '((2 1 3 2 3 3 2 3 3 3) (1 3 2 2 1 3 3 2 2 2) (1 3 1 3 2 2 2 1 2 1) (1 3 3 3 1 3 1 1 1 3)) "abordagem.alternativa"))

;(print (resolve-same-game '((4 3 3 1 2 5 1 2 1 5) (2 4 4 4 1 5 2 4 1 2) (5 2 4 1 4 5 1 2 5 4) (1 3 1 4 2 5 2 5 4 5)) "a*.melhor.heuristica"))

;(print (resolve-same-game '((3 3 3 2 1 2 3 1 3 1) (1 1 2 3 3 1 1 1 3 1) (3 3 1 2 1 1 3 2 1 1) (3 3 2 3 3 1 3 3 2 2) (3 2 2 2 3 3 2 1 2 2) (3 1 2 2 2 2 1 2 1 3) (2 3 2 1 2 1 1 2 2 1) (2 2 3 1 1 1 3 2 1 3) (1 3 3 1 1 2 3 1 3 1) (2 1 2 2 1 3 1 1 2 3) (2 1 1 3 3 3 1 2 3 1) (1 2 1 1 3 2 2 1 2 2) (2 1 3 2 1 2 1 3 2 3) (1 2 1 3 1 2 2 3 2 3) (3 3 1 2 3 1 1 2 3 1)) "a*.melhor.heuristica"))
