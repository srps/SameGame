;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;			Procura e Planeamento 2012-2013                 ;;;
;;;			Projecto Same Game	                        ;;;
;;;									;;;
;;;			Grupo 002				        ;;;
;;;			55457 - Sergio Miguel Silva			;;;
;;;			56886 - Marco Andre Ferreira			;;;
;;;									;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :user)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;	DEFINICOES  DE CONSTANTES 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defconstant MAX-TEMPO 280) ;;Tempo limite de tempo para execução



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;	TEMPO
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;--------------------------------------------------------------------------;
; Função que calcula tempo restante                                        ;
;--------------------------------------------------------------------------;
; ARG1 - tempo inicia                                                      ;
; ARG2 - segundos passados                                                 ;
;--------------------------------------------------------------------------;             
(defun time-to-stop? (tempo-inicio n-segundos)
        (<= (* n-segundos INTERNAL-TIME-UNITS-PER-SECOND) (- (get-start-time) tempo-inicio)))

;--------------------------------------------------------------------------;
; Função que retorna o tempo interno actual                                ;
;--------------------------------------------------------------------------;
; ARG1 - tempo inicia                                                      ;
; ARG2 - segundos passados                                                 ;
;--------------------------------------------------------------------------; 
(defun get-start-time ()
                (get-internal-run-time))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;	DEFINICOES  e  ESTRUTURAS  DE  DADOS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *tempo-inicial* (get-internal-run-time))
(defvar *tamanho-tabuleiro* 0)
(defvar *max-prof* 0)
(defvar *result*)


(defstruct bloco
  (cor -1 :type fixnum)       ; cor do bloco
  lista-pecas                 ; lista das peças do bloco
  (id -1 :type fixnum)        ; identificador do bloco
  (x-min 20 :type unsigned-byte)
  (x-max 0 :type unsigned-byte)
  (y-min 20 :type unsigned-byte)
  (y-max 0 :type unsigned-byte)
)

	
(defstruct nos
  (pontuacao 0 :type unsigned-byte) ; Pontuação até ao momento do estado
  (prof 0 :type unsigned-byte)
  (n-pecas 0 :type unsigned-byte)    ; Peças por eliminar
  (n-blocos 0 :type unsigned-byte)   ; Blocos por eliminar         
  tabuleiro
  h-blocos                    ; Hash com os blocos existentes	
  (n-linhas 0 :type unsigned-byte)   ; Numero de linhas com peças
  (n-colunas 0 :type unsigned-byte)  ; Numero de colunas com peças
  (maior-bloco 0 :type unsigned-byte); Tamanho do maior bloco
  pecas-removidas
  (h 0 :type unsigned-byte)
)

(defstruct result
  (pontuacao 0)
  (pecas-removidas (list)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                               
;;   FUNCAO ESTADO-OBJECTIVO   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                              
(defun objectivo? (estado)
 (time-to-stop? *tempo-inicial* MAX-TEMPO))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;	DEBUG
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun print-tabuleiro (tabuleiro n-lin n-col)
  (declare (unsigned-byte n-lin n-col))
  (let* ((result '())
         (temp-list '()))
    (loop for posy from 0 to (- n-lin 1) do
          (loop for posx from 0 to (- n-col 1) do
                (if (not (eq nil (nth posx (nth posy tabuleiro))))
                    (setq temp-list (append temp-list (list (car (nth posx (nth posy tabuleiro))))))
                  (setq temp-list (append temp-list (list -1))))) ; Caso Não exista peça coloca -1
          (setq result (append result (list temp-list)))
          (setf temp-list '()))
  (print result)))

(defun print-hash (hash)
  (let* ((b-aux (make-bloco)))
 (loop for key being the hash-keys of hash do
       (setf b-aux (gethash key hash))
       (format t "~% Key: ~D Cor Bloco: ~D Numero De Peças: ~D Xmin: ~D Xmax: ~D Ymin: ~D Ymax: ~D Lista Peças: ~A" 
               key (bloco-cor b-aux) (list-length (bloco-lista-pecas b-aux)) 
               (bloco-x-min b-aux) (bloco-x-max b-aux) (bloco-y-min b-aux) 
               (bloco-y-max b-aux) (bloco-lista-pecas b-aux)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;	SUCESSORES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;--------------------------------------------------------------------------;
; Função que gera sucessores                                               ;
;--------------------------------------------------------------------------;
; ARG1 - estado                                                            ;
;--------------------------------------------------------------------------;
; RET  - Devolve a lista com os estados sucessores                         ;
;--------------------------------------------------------------------------;

(defun gera-sucessores (estado)
  (let* ((hash (nos-h-blocos estado))
         (lista (list)))
    (loop for key being the hash-keys of hash do     
          (let* ((novo-estado (copia-estado estado))
                 (b-aux (gethash key (nos-h-blocos novo-estado)))
                 (tab (nos-tabuleiro novo-estado))
                 (ht (nos-h-blocos novo-estado)))
            (if (> (nos-prof novo-estado) *max-prof*)
                (setf *max-prof* (nos-prof novo-estado)))
            (if (>= (list-length (bloco-lista-pecas b-aux)) 2)
                (progn                  
                  (atualiza-tabuleiro tab ht)                  
                  (remove-bloco novo-estado key ht)                  
                  (let* ((l-margens
                          (gravidade tab b-aux ht)))
                    (encosta-esquerda novo-estado tab ht)
                    (setf ht (lista-blocos tab 
                                           0 (car l-margens) 
                                           0 (cdr l-margens)
                                           (nos-n-linhas novo-estado) (nos-n-colunas novo-estado) ht)))
                  (maior-bloco novo-estado ht)
                  (incf (nos-prof novo-estado))
                  (push novo-estado lista)
            ))))
    lista))

;--------------------------------------------------------------------------;
; Função que gera sucessores alternativa, com cortes                       ;
;--------------------------------------------------------------------------;
; ARG1 - estado                                                            ;
;--------------------------------------------------------------------------;
; RET  - Devolve a lista com os estados sucessores                         ;
;--------------------------------------------------------------------------;

(defun gera-sucessores-alternativo (estado)
  (let* ((hash (nos-h-blocos estado))
         (lista (list)))
    (loop for key being the hash-keys of hash do     
          (let* ((novo-estado (copia-estado estado))
                 (b-aux (gethash key (nos-h-blocos novo-estado)))
                 (tab (nos-tabuleiro novo-estado))
                 (ht (nos-h-blocos novo-estado)))
            (if (> (nos-prof novo-estado) *max-prof*)
                (setf *max-prof* (nos-prof novo-estado)))
            (if (>= (list-length (bloco-lista-pecas b-aux)) 2)
                (progn                  
                  (atualiza-tabuleiro tab ht)                  
                  (remove-bloco novo-estado key ht)                  
                  (let* ((l-margens
                          (gravidade tab b-aux ht)))
                    (encosta-esquerda novo-estado tab ht)
                    (setf ht (lista-blocos tab 
                                           0 (car l-margens) 
                                           0 (cdr l-margens)
                                           (nos-n-linhas novo-estado) (nos-n-colunas novo-estado) ht)))
                  (maior-bloco novo-estado ht)
                  (incf (nos-prof novo-estado))
                  (setf (nos-h novo-estado) (funcall *heuristica* novo-estado))
                  (push novo-estado lista)
            ))))
    (particiona-sucessores (sort lista #'< :key #'nos-h) 1)))
  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;	HEURISTICAS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;--------------------------------------------------------------------------;
; Heuristica  que dá mais prioridade a retirar blocos pequenos             ;
;--------------------------------------------------------------------------;
; ARG1 - estado                                                            ;
;--------------------------------------------------------------------------;
(defun heuristicaMenorBloco (estado)
  (- *tamanho-tabuleiro* (nos-n-pecas estado)))


;-------------------------------------------------------------------------------------------------------------------------------;
; Extensão à  heuristicaMenorBloco que após certa profundidade dá prioridade a retirar blocos que maximizem a pontuação máxima  ;
;-------------------------------------------------------------------------------------------------------------------------------;
; ARG1 - estado                                                                                                                 ;
;-------------------------------------------------------------------------------------------------------------------------------;
(defun heuristicaPrincipal (estado)
  (let* ((result 0))
    (if (and (> (nos-prof estado) 3)
             (> (expt (- (nos-maior-bloco estado) 2) 2) (result-pontuacao *result*)))
        (setf result (- *tamanho-tabuleiro* (nos-maior-bloco estado)))
      (setf result (+ (* 150 (- *tamanho-tabuleiro* (nos-prof estado))) (heuristicaMenorBloco estado))))
    (if (< result 0)
        (setf result 0))
    result))



;--------------------------------------------------------------------------;
; Heuristica  que dá mais prioridade a retirar blocos grandes              ;
; Efectua uma procura em profundidade                                      ; 
;--------------------------------------------------------------------------;
; ARG1 - estado                                                            ;
;--------------------------------------------------------------------------;
(defun heuristicaMenorTabuleiro (estado)
  (nos-n-pecas estado))



(defvar *heuristica* #'heuristicaPrincipal)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   SONDAGEM ITERATIVA   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;            
                
;recebe uma lista de sucessores não vazia
;devolve um dos sucessores da lista, escolhido aleatóriamente           
(defun escolhe-sucessor-random (lst-sucessores)
        (let ((n-random (random (length lst-sucessores))))
                (nth n-random lst-sucessores)))

;faz uma iteracao de sondagem iteratica a partir do estado dado
;devolve o ultimo estado que conseguiu chegar na iteracao
(defun iteracao-sondagem-iterativa (estado)
        (let ((lst-sucessores ()))
                (if (objectivo? estado)
                        (return-from iteracao-sondagem-iterativa)
                        (progn  (setf lst-sucessores (gera-sucessores estado))
                                        (if (null lst-sucessores)
                                                (return-from iteracao-sondagem-iterativa)
                                                (iteracao-sondagem-iterativa (escolhe-sucessor-random lst-sucessores)))))))                             
	

	
;Algoritmo sondagem iterativa na perspectiva de optimização     
(defun sondagem-iterativa (estado)
                (let ((tempo-inicio (get-start-time))
                                (melhor-solucao nil)
                                (estado-solucao nil))
                        (loop
                                (if (time-to-stop? tempo-inicio MAX-TEMPO)
                                    (return)
                                  (iteracao-sondagem-iterativa estado))
                                   )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;	FUNÇÔES AUXILIARES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;--------------------------------------------------------------------;
; Função que transforma uma lista 2D num array 2D                    ;
; -------------------------------------------------------------------;
; ARG1 - numero de linhas                                            ;
;--------------------------------------------------------------------;

(defun particiona-sucessores (list int)
  (declare (unsigned-byte media))
  (let ((resul list))
    (loop for l in list do
          (if (zerop int)
              (return-from particiona-sucessores resul)
            (decf int))
        (push l resul))
    resul))

;--------------------------------------------------------------------;
; Função que transforma uma lista 2D num array 2D                    ;
; -------------------------------------------------------------------;
; ARG1 - numero de linhas                                            ;
;--------------------------------------------------------------------;

(defun list-to-2d-array (list)
  (make-array (list (length list)
                    (length (first list)))
              :initial-contents list))

;--------------------------------------------------------------------;
; Função que transforma um array 2D numa lista 2D                    ;
; -------------------------------------------------------------------;
; ARG1 - numero de linhas                                            ;
; ARG2 - numero de colunas                                           ;
;--------------------------------------------------------------------;

(defun 2d-array-to-list (array)
  (loop for i below (array-dimension array 0)
        collect (loop for j below (array-dimension array 1)
                      collect (aref array i j))))

;--------------------------------------------------------------------;
; Função que atualiza o tabuleiro a partir da hash                   ;
; -------------------------------------------------------------------;
; ARG1 - Tabuleiro                                                   ;
; ARG2 - Hash Table                                                  ;
;--------------------------------------------------------------------;

(defun atualiza-tabuleiro (tabuleiro ht)
  (loop for bl being the hash-values of ht do
        (loop for p-pos in (bloco-lista-pecas bl) do
              (setf (aref tabuleiro (cdr p-pos) (car p-pos)) 
                    (cons (bloco-cor bl) (bloco-id bl))))))

;--------------------------------------------------------------------------;
; Função que cria uma hash table nova a partir de uma existente            ;
;--------------------------------------------------------------------------;
; ARG1 - Hash Table                                                        ;
;--------------------------------------------------------------------------;

(defun copia-hash (hash)
  (let* ((new-hash (make-hash-table))
         (b-aux (make-bloco))
         (l-aux (list)))
    (loop for key being the hash-keys of hash do     
          (setf b-aux (gethash key hash))
          (loop for p-pos in (bloco-lista-pecas b-aux) do
                (push (cons (car p-pos) (cdr p-pos)) l-aux))
          (setf (gethash key new-hash) (make-bloco :cor (bloco-cor b-aux) 
                                       :lista-pecas l-aux 
                                       :id (bloco-id b-aux) 
                                       :x-min (bloco-x-min b-aux) 
                                       :x-max (bloco-x-max b-aux)
                                       :y-min (bloco-y-min b-aux)
                                       :y-max (bloco-y-max b-aux)))
          (setf l-aux (list)))    
    new-hash))
  


;--------------------------------------------------------------------------;
; Função que devolve a maior chave da hash table                           ;
;--------------------------------------------------------------------------;
; ARG1 - Hash Table                                                        ;
;--------------------------------------------------------------------------;

(defun ve-maior-hash (ht)
  (let* ((resul -1))
    (loop for k being the hash-keys of ht do
          (when (> k resul) (setq resul k)))
    resul))


;--------------------------------------------------------------------------;
; Função que efectua a cópia de um estado                                  ;
;--------------------------------------------------------------------------;
; ARG1 - estado                                                            ;
;--------------------------------------------------------------------------;


(defun copia-estado (estado)
  (let* ((pr-aux (list)))
  (loop for p-pos in (nos-pecas-removidas estado) do
                (push (cons (car p-pos) (cdr p-pos)) pr-aux))  
  (make-nos :tabuleiro (make-array (list (nos-n-linhas estado) (nos-n-colunas estado)))
            :h-blocos (copia-hash (nos-h-blocos estado))
            :prof (nos-prof estado)
            :pontuacao (nos-pontuacao estado)
            :n-pecas (nos-n-pecas estado)
            :n-blocos (nos-n-blocos estado)
            :n-linhas (nos-n-linhas estado)
            :n-colunas (nos-n-colunas estado)
            :maior-bloco 0
            :pecas-removidas pr-aux)))
                                                                      



;--------------------------------------------------------------------------;
; Função que devolve o tamanho do maior bloco da hash                      ;
;--------------------------------------------------------------------------;
; ARG1 - estado                                                            ;
; ARG2 - hash                                                              ;
;--------------------------------------------------------------------------;

(defun maior-bloco (estado hash)
  (let* ((result 0)
          (b-aux))
    (declare (unsigned-byte result tmp))
          (loop for key being the hash-keys of hash do
                (setf b-aux (gethash key hash))
                (if (< result (list-length (bloco-lista-pecas b-aux)))
                    (setf result (list-length (bloco-lista-pecas b-aux)))))
          (setf (nos-maior-bloco estado) result)))

;--------------------------------------------------------------------------;
; Função que remove bloco do tabuleiro e hash table e atualiza a pontuação ;
;--------------------------------------------------------------------------;
; ARG1 - Estado (nó)                                                       ;
; ARG2 - ID do bloco a ser removido                                        ;
; ARG3 - Hash table                                                        ;
;--------------------------------------------------------------------------;

(defun remove-bloco (estado id-bloco ht)
  (declare (unsigned-byte id-bloco))
  (let* ((l-aux (bloco-lista-pecas (gethash id-bloco ht)))
         (pontos (expt (- (list-length l-aux) 2) 2)))
    (setf (nos-pecas-removidas estado) (push (first l-aux) (nos-pecas-removidas estado) ))
    (remhash id-bloco ht)
    (setf (nos-pontuacao estado) (+ (nos-pontuacao estado) pontos))
    (setf (nos-n-pecas estado) (- (nos-n-pecas estado) (list-length l-aux)))
    (loop for pos in l-aux do
          (setf (aref (nos-tabuleiro estado) (cdr pos) (car pos)) NIL))
          (if (> (nos-pontuacao estado) (result-pontuacao *result*))
              (progn
                  (setf (result-pontuacao *result*) (nos-pontuacao estado))
                  (setf (result-pecas-removidas *result*)(nos-pecas-removidas estado))
                   ))))

;--------------------------------------------------------------------------;
; Função que faz cair as peças consoante as leis da gravidade              ;
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
         (resul (cons x-fin y-ini)))
    (declare (unsigned-byte x-ini x-fin y-ini x-fin y-ini contador))
    (loop for coluna from x-ini to x-fin do                      ; Para evitar ver peças desnecessárias no lista-blocos
          (loop for linha from y-ini downto 0 do                     ; --Y min não interessa porque as peças caem
                (setq p-aux (aref tabuleiro linha coluna))
                (if (not (eq p-aux NIL))                                                     ; Se houver peça na posição indicada
                    (if (> contador 0)                                                       ; Se houver espaços vazios abaixo da peça
                        (progn                                                               ; --
                          (setf bl-aux (cdr p-aux))                                          ; -- Vê o bloco da peça que vai ser deslocada para baixo
                          (if (not (= bl-aux -1))                                            ; -- Se existir (vai tratar do bloco) 
                              (progn                                                         ; ----
                                (setf b-aux (gethash bl-aux ht))                             ; ---- Guarda o bloco para ser acedido facilmente
                                (if (> (bloco-x-max b-aux) (car resul))                   ; ----
                                      (setf (car resul) (bloco-x-max b-aux)))               ; ------
                                (if (> (bloco-y-max b-aux) (cdr resul))                    ; ----
                                      (setf (cdr resul) (bloco-y-max b-aux)))                ; ------
                                (loop for p-pos in (bloco-lista-pecas b-aux) do              ; ---- Para cada peça do bloco a ser removido
                                      (rplacd (aref tabuleiro (cdr p-pos) (car p-pos)) -1))   ; Remove o bloco das peças
                                (remhash bl-aux ht)))                                                ; --Remove o bloco da hash
                          (setf (aref tabuleiro linha coluna) NIL)                      ; Atualiza o tabuleiro
                          (setf (aref tabuleiro (+ linha contador) coluna) p-aux)))     ; Atualiza o tabuleiro
                  (incf contador)))                                                          ; Se for uma posição vazia, incrementa o contador
          (setq contador 0))                                                                 ; Reset do contador a cada coluna nova
    resul))                                                               


;---------------------------------------------------------------------------;
; Função que encosta as peças à esquerda, eliminando colunas vazias no meio ;
;---------------------------------------------------------------------------;
; ARG1 - estado                                                             ;
; ARG2 - Tabuleiro do jogo                                                  ;
; ARG3 - Hash table                                                         ;
;---------------------------------------------------------------------------;
; RET  - Nº de shifts que efetuou                                           ;
;---------------------------------------------------------------------------;


(defun encosta-esquerda (estado tabuleiro ht)
  (let* ((x-fin (- (array-dimension tabuleiro 1) 1))
         (y-ini (- (array-dimension tabuleiro 0) 1))
         (p-aux)
         (contador 0))
    (declare (unsigned-byte x-fin y-ini contador))
    (loop for coluna from 0 to x-fin do
          (if (eq (aref tabuleiro y-ini coluna) NIL)
              (incf contador)                                                                         ; Se for uma posição vazia, incrementa o contador
            (if (> contador 0)
                (loop for linha from y-ini downto 0 do
                      (setq p-aux (aref tabuleiro linha coluna))
                      (if (not (eq p-aux NIL))                                                        ; Se houver peça na posição indicada
                          (progn
                            (setf (aref tabuleiro linha coluna) NIL)                             ; Atualiza o tabuleiro
                            (setf (aref tabuleiro linha (- coluna contador)) p-aux)              ; Atualiza o tabuleiro
                            (if (not (= (cdr p-aux) -1))
                                (progn
                                  (let* ((b-aux (gethash (cdr p-aux) ht))
                                         (l-aux (list)))
                                    (loop for p in (bloco-lista-pecas b-aux) do
                                          (if (equal p (cons coluna linha))
                                              (push (cons (- coluna contador) linha) l-aux)
                                              (push p l-aux)))
                                    (setf (bloco-lista-pecas (gethash (cdr p-aux) ht)) l-aux)
                                    (setq l-aux (list))
                                    (if (< (- coluna contador) (bloco-x-min b-aux))
                                        (setf (bloco-x-min (gethash (cdr p-aux) ht)) (- coluna contador)))                                  
                                    (if (>= coluna (bloco-x-max b-aux))
                                        (setf (bloco-x-max (gethash (cdr p-aux) ht)) (- coluna contador)))))))                       
                        (return))))))                                                                 ; Quando vê NIL, salta para a próxima coluna
    (setf (nos-n-colunas estado) (- (nos-n-colunas estado) contador))))      



;----------------------------------------;
; Função que junta 2 blocos da mesma cor ;
; Mantém o 1º bloco que é passado        ;
; ---------------------------------------;
; ARG1 - tabuleiro com as peças          ;
; ARG2 - hashtable dos blocos            ;
; ARG3 - chave do bloco a manter         ;
; ARG4 - chave do bloco que desaparece   ;
;----------------------------------------;

(defun junta-blocos (tabuleiro ht chave-b1 chave-b2)
  (declare (unsigned-byte chave-b1 chave-b2))
  (let* ((b-aux (gethash chave-b1 ht))                                   ; Referência para o bloco que se vai manter
         (b-trash (gethash chave-b2 ht))                                 ; Referência para o bloco que vai à vida
         (l-aux (bloco-lista-pecas b-aux))                               ; Lista das peças do bloco que se vai manter
         (xmin (bloco-x-min b-trash))                                    ; X Mínimo do bloco que vai à vida
         (xmax (bloco-x-max b-trash))                                    ; X Máximo do bloco que vai à vida
         (ymin (bloco-y-min b-trash))                                    ; Y Mínimo do bloco que vai à vida
         (ymax (bloco-y-max b-trash)))                                   ; Y Máximo do bloco que vai à vida
    (declare (unsigned-byte xmin xmax ymin ymax))
  (loop for p-pos in (bloco-lista-pecas b-trash) do
        (rplacd (aref tabuleiro (cdr p-pos) (car p-pos))
              chave-b1)                                                  ; Muda o bloco da peça
        (push (cons (car p-pos) (cdr p-pos)) l-aux))                     ; Insere a peça na lista do bloco original       
  (setf (bloco-lista-pecas b-aux) l-aux)                                 ; Coloca a nova lista no bloco original
  (if (> (bloco-x-min b-aux) xmin)                                       ;;---------------------------------------------;
      (setf (bloco-x-min b-aux) xmin))                                   ;;                                             ;
  (if (< (bloco-x-max b-aux) xmax)                                       ;;                                             ;
      (setf (bloco-x-max b-aux) xmax))                                   ;; Atualiza os mínimos e máximos do bloco      ;
  (if (> (bloco-y-min b-aux) ymin)                                       ;;                                             ;
      (setf (bloco-y-min b-aux) ymin))                                   ;;                                             ;
  (if (< (bloco-y-max b-aux) ymax)                                       ;;                                             ;
      (setf (bloco-y-max b-aux) ymax))                                   ;;---------------------------------------------;
  (setf (gethash chave-b1 ht) b-aux)                                     ; Atualiza o bloco original na HT
  (remhash chave-b2 ht)                                                  ; Remove o 2º bloco da HT  
))                                                


;---------------------------------------------;
; Função que percorre uma determinada zona    ;
; e determina os diferentes blocos existentes ;
; --------------------------------------------;
; ARG1 - tabuleiro com as peças               ;
; ARG2 - limite esquerda                      ;
; ARG3 - limite direita                       ;
; ARG4 - limite cima                          ;
; ARG5 - limite baixo                         ;
; ARG6 - numero linhas do tabuleiro           ;
; ARG7 - numero colunas do tabuleiro          ;
; ARG8 - hash com bloocs                      ;
;---------------------------------------------;

(defun lista-blocos (tabuleiro x-ini x-fin y-ini y-fin n-lin n-col ht)
  (declare (unsigned-byte x-ini x-fin y-ini y-fin n-lin n-col))
  (let* ((p-aux)
         (b-aux)
         (contador (+ (ve-maior-hash ht) 1)))
    (declare (unsigned-byte contador))
    (loop for posx from x-ini to x-fin do
          (loop for posy from y-ini to y-fin do
                (if (not (eq (aref tabuleiro posy posx) nil))
                    (progn
                      (setq p-aux (aref tabuleiro posy posx))
                      (if (= (cdr p-aux) -1)                                          ; Vê se a peça já está num bloco                          
                          (progn                                                             ; Se não estiver num bloco
                            (rplacd p-aux contador)                               ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                            (setq b-aux (make-bloco :cor (car p-aux)                    ;;
                                                    :lista-pecas (list (cons posx posy))     ;;
                                                    :id contador                             ;;
                                                    :x-min posx                              ;;
                                                    :x-max posx                              ;;
                                                    :y-min posy                              ;;
                                                    :y-max posy))                            ;; Cria um bloco para a peça
                            (setf (gethash (cdr p-aux) ht) b-aux)                  ;; coloca-a no bloco
                            (setf (aref tabuleiro posy posx) p-aux)                     ;; e guarda o bloco para referência
                            (incf contador)))
                      (if (and (not (>= posx (- n-col 1)))                                   ; Estou na última coluna do tabuleiro?
                               (not (eq (aref tabuleiro posy (+ posx 1)) nil)))         ; A peça à frente existe?
                          (ve-frente tabuleiro p-aux posx posy ht))                 ; --Se não, verifica bloco à direita
                      (if (not (>= posy (- n-lin 1)))                                        ; Estou na última linha do tabuleiro?                                       
                          (ve-abaixo tabuleiro p-aux posx posy ht))))))
    ht))
 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;	FUNÇÔES DE LEITURA DE TABULEIRO INICIAL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;        


;--------------------------------------------------------------------;
; Função que gera peças e constrói um tabuleiro novo                 ;
; -------------------------------------------------------------------;
; ARG1 - numero de linhas                                            ;
; ARG2 - numero de colunas                                           ;
;--------------------------------------------------------------------;

(defun cria-tabuleiro-novo (n-lin n-col)
  (declare (unsigned-byte n-lin n-col))
  (let* ((resul (list))
         (l-aux (list)))
    (loop for linha from 0 to (- n-lin 1) do
          (loop for coluna from 0 to (- n-col 1) do
                (setq l-aux (append l-aux (list NIL))))
          (setq resul (append resul (list l-aux)))
          (setq l-aux (list)))
    resul))


;--------------------------------------------------------------------;
; Função que gera peças e constrói um tabuleiro a partir do original ;
; -------------------------------------------------------------------;
; ARG1 - tabuleiro com lista de cores recebido no input              ;
; ARG2 - numero de colunas                                           ;
;--------------------------------------------------------------------;

(defun cria-tabuleiro (tabuleiro n-col)
  (declare (unsigned-byte n-col))
  (let* ((resul (list))
         (posx 0)
         (posy 0)
         (p-aux)
         (l-aux (list)))
    (declare (unsigned-byte posx posy))
    (loop for linha in tabuleiro do
          (loop for coluna in linha do
                (if (= coluna -1)
                    (setq p-aux nil)
                  (setq p-aux (cons coluna -1)))
                (setq l-aux (append l-aux (list p-aux)))
                (if (not (= posx (- n-col 1)))                                       ; Avança no Y caso não esteja no final da linha
                    (incf posx)               
                  (progn                                                             ; Se estiver no Final, desce 1 linha e faz reset no posx
                    (setq resul (append resul (list l-aux)))
                    (setq l-aux (list))
                    (setf posx 0)          
                    (incf posy)))))
                resul))
                      


;----------------------------------------------------------------------;
; Função que verifica se a peça à direita pertence ao mesmo bloco      ;
; ---------------------------------------------------------------------;
; ARG1 - tabuleiro com as peças                                        ;
; ARG2 - peça a partir da qual se verifica se pertence ao mesmo bloco  ;
; ARG3 - coord.x da posicao da peça p-aux                              ;
; ARG4 - coord.y da posicao da peça p-aux                              ;
; ARG5 - hash com blocos                                               ;
;----------------------------------------------------------------------;

(defun ve-frente (tabuleiro p-aux posx posy ht)
  (declare (unsigned-byte posx posy))
  (let* ((p-dir (aref tabuleiro posy (+ posx 1)))
         (chave-b1 (cdr p-aux))
         (l-aux (bloco-lista-pecas (gethash chave-b1 ht)))
         (chave-b2 (cdr p-dir)))
    (declare (unsigned-byte chave-b1 chave-b2))
    (if (= (car p-aux) (car p-dir))                                                   ; Se o da frente for igual
        (if (= -1 chave-b2)
            (progn
              (rplacd p-dir chave-b1)                                                 ; Junta a informação do bloco à peça da direita
              (setf (aref tabuleiro posy (+ posx 1)) p-dir)                      ; Coloca a peça atualizada no tabuleiro
              (push (cons (+ posx 1) posy) l-aux )                                    ; Adiciona a peça à lista para atualizar o bloco
              (setf (bloco-lista-pecas (gethash chave-b1 ht)) l-aux)                  ; Atualiza o bloco na hash
              (if (> (+ posx 1) (bloco-x-max (gethash chave-b1 ht)))                  ; Se a peça adicionada tiver x maior que o máximo do bloco
                  (setf (bloco-x-max (gethash chave-b1 ht)) (+ posx 1))))             ; --Incrementa o xmax do bloco     
          (if (not (= chave-b1 chave-b2))
              (if (>= (list-length (bloco-lista-pecas (gethash chave-b1 ht))) 
                      (list-length (bloco-lista-pecas (gethash chave-b2 ht))))
                  (junta-blocos tabuleiro ht chave-b1 chave-b2)
                (junta-blocos tabuleiro ht chave-b2 chave-b1)))))
))


;----------------------------------------------------------------------;
; Função que verifica se a peça abaixo pertence ao mesmo bloco         ;
; ---------------------------------------------------------------------;
; ARG1 - tabuleiro com as peças                                        ;
; ARG2 - peça a partir da qual se verifica se pertence ao mesmo bloco  ;
; ARG3 - coord.x da posicao da peça p-aux                              ;
; ARG4 - coord.y da posicao da peça p-aux                              ;
; ARG5 - hash com blocos                                               ;
;----------------------------------------------------------------------;

(defun ve-abaixo (tabuleiro p-aux posx posy ht)
  (declare (unsigned-byte posx posy))
  (let* ((p-baixo (aref tabuleiro (+ posy 1) posx))
         (chave-b1 (cdr p-aux))
         (l-aux (bloco-lista-pecas (gethash chave-b1 ht)))
         (chave-b2 (cdr p-baixo)))
    (declare (unsigned-byte chave-b1 chave-b2))
    (if (= (car p-aux) (car p-baixo))                                                   ; Se o da frente for igual
        (if (= -1 chave-b2)
            (progn             
              (rplacd p-baixo chave-b1)                                              ; Junta a informação do bloco à peça da direita
              (setf (aref tabuleiro (+ posy 1) posx) p-baixo)                      ; Coloca a peça atualizada no tabuleiro
              (push (cons posx (+ posy 1)) l-aux)                                       ; Adiciona a peça à lista para atualizar o bloco
              (setf (bloco-lista-pecas (gethash chave-b1 ht)) l-aux)                    ; Atualiza o bloco na hash
              (if (> (+ posy 1) (bloco-y-max (gethash chave-b1 ht)))                    ; Se a peça adicionada tiver y maior que o máximo do bloco
                  (setf (bloco-y-max (gethash chave-b1 ht)) (+ posy 1))))               ; --Incrementa o ymax do bloco 
          (if (not (= chave-b1 chave-b2))
              (if (>= (list-length (bloco-lista-pecas (gethash chave-b1 ht))) 
                      (list-length (bloco-lista-pecas (gethash chave-b2 ht))))
                  (junta-blocos tabuleiro ht chave-b1 chave-b2)
                (junta-blocos tabuleiro ht chave-b2 chave-b1)))))
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;	FUNÇÂO PRINCIPAL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 


(defun resolve-same-game (problema algoritmo)
  (let* ((tab-array (list-to-2d-array (cria-tabuleiro problema (list-length (first problema)))))
         (h-blocos (lista-blocos tab-array 0 (- (array-dimension tab-array 1) 1) 0 (- (array-dimension tab-array 0) 1) (array-dimension tab-array 0) (array-dimension tab-array 1) (make-hash-table)))
         (estado-inicial (make-nos :n-pecas (* (array-dimension tab-array 0) (array-dimension tab-array 1)) :n-blocos (hash-table-count h-blocos) :tabuleiro tab-array :h-blocos h-blocos :n-linhas (array-dimension tab-array 0) :n-colunas (array-dimension tab-array 1) :maior-bloco 0)))
    (setf *tamanho-tabuleiro* (*  (array-dimension tab-array 1) (array-dimension tab-array 0)))
    (setf *tempo-inicial* (get-internal-run-time))
    (setf *result* (make-result))

    (cond ((string-equal algoritmo "melhor.abordagem")
                 (procura-tabuleiro estado-inicial (list #'gera-sucessores) heuristica1))

                ((string-equal algoritmo "a*.melhor.heuristica")
                 (time (procura (cria-problema estado-inicial (list #'gera-sucessores) :objectivo? #'objectivo? :custo (always 0) :heuristica #'heuristicaPrincipal) "a*" :espaco-em-arvore? T)))

                ((string-equal algoritmo "a*.melhor.heuristica.alternativa")
                 (time (procura (cria-problema estado-inicial (list #'gera-sucessores) :objectivo? #'objectivo? :custo (always 0) :heuristica #'heuristicaMenorTabuleiro) "a*" :espaco-em-arvore? T)))

                ((string-equal algoritmo "sondagem.iterativa")
                 (time (sondagem-iterativa estado-inicial)))

                ((string-equal algoritmo "abordagem.alternativa")
                 (time (procura (cria-problema estado-inicial (list #'gera-sucessores-alternativo) :objectivo? #'objectivo? :estado= #'equal) 
									"largura" :espaco-em-arvore? T)))


                ;; Para efeitos de Teste. Não fazem parte das 5 estratégias pedidas
                ((string-equal algoritmo "profundidade")
                 (time (procura (cria-problema estado-inicial (list #'gera-sucessores) :objectivo? #'objectivo? :estado= #'equal) 
									"profundidade" :espaco-em-arvore? T)))
                
                
                ((string-equal algoritmo "largura")
                 (time (procura (cria-problema estado-inicial (list #'gera-sucessores) :objectivo? #'objectivo? :estado= #'equal) 
									"largura" :espaco-em-arvore? T)))
                )
    (setf tamanho-tabuleiro 0)
  *result*))

; S5
;(print (resolve-same-game '((2 1 3 2 3 3 2 3 3 3) (1 3 2 2 1 3 3 2 2 2) (1 3 1 3 2 2 2 1 2 1) (1 3 3 3 1 3 1 1 1 3)) "profundidade"))

; S10
;(print (resolve-same-game '((4 3 3 1 2 5 1 2 1 5) (2 4 4 4 1 5 2 4 1 2) (5 2 4 1 4 5 1 2 5 4) (1 3 1 4 2 5 2 5 4 5)) "a*.melhor.heuristica"))

; S15
(print (resolve-same-game '((3 3 3 2 1 2 3 1 3 1) (1 1 2 3 3 1 1 1 3 1) (3 3 1 2 1 1 3 2 1 1) (3 3 2 3 3 1 3 3 2 2) (3 2 2 2 3 3 2 1 2 2) (3 1 2 2 2 2 1 2 1 3) (2 3 2 1 2 1 1 2 2 1) (2 2 3 1 1 1 3 2 1 3) (1 3 3 1 1 2 3 1 3 1) (2 1 2 2 1 3 1 1 2 3) (2 1 1 3 3 3 1 2 3 1) (1 2 1 1 3 2 2 1 2 2) (2 1 3 2 1 2 1 3 2 3) (1 2 1 3 1 2 2 3 2 3) (3 3 1 2 3 1 1 2 3 1)) "abordagem.alternativa"))

; S20
;(print (resolve-same-game '((5 1 1 1 2 1 4 2 1 2) (5 5 5 4 1 2 2 1 4 5) (5 5 3 5 5 3 1 5 4 3) (3 3 3 2 4 3 1 3 5 1) (5 3 4 2 2 2 2 1 3 1) (1 1 5 3 1 1 2 5 5 5) (4 2 5 1 4 5 4 1 1 1) (5 3 5 3 3 3 3 4 2 2) (2 3 3 2 5 4 3 4 4 4) (3 5 5 2 2 5 2 2 4 2) (1 4 2 3 2 4 5 5 4 2) (4 1 3 2 4 3 4 4 3 1) (3 1 3 4 4 1 5 1 5 4) (1 3 1 5 2 4 4 3 3 2) (4 2 4 2 2 5 3 1 2 1)) "a*.melhor.heuristica"))
