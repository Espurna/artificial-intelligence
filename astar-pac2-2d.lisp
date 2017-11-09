;; (load "astar.lisp")

;;;; En aquest fitxer no hi ha gaires comentaris perquè el codi
;;;; ja està explicat amb molt de detall als apartats corresponents
;;;; del Mòdul 2.

;;;;;;;;;;;;; quicksort ;;;;;;;;;;;;

(defun selecciona-estimacio (unio-nodes)
  (mapcar #'(lambda (node) (caddr (cdddr node)))
            unio-nodes))

(defun selecciona-menorigual (pivot per-ordenar elems)
  (if (null per-ordenar) nil
    (if (<= (car per-ordenar) pivot)
        (cons (car elems)
              (selecciona-menorigual pivot (cdr per-ordenar)
                                     (cdr elems)))
      (selecciona-menorigual pivot (cdr per-ordenar) (cdr elems)))))

(defun selecciona-major (pivot per-ordenar elems)
  (if (null per-ordenar) nil
    (if (> (car per-ordenar) pivot)
        (cons (car elems)
              (selecciona-major pivot (cdr per-ordenar)
                                (cdr elems)))
      (selecciona-major pivot (cdr per-ordenar) (cdr elems)))))

(defun quicksort (per-ordenar elems)
  (if (null elems) nil
    (let* ((pivot (car per-ordenar)) (elemp (car elems))
           (petits (selecciona-menorigual pivot (cdr per-ordenar) (cdr elems)))
           (grans (selecciona-major pivot (cdr per-ordenar) (cdr elems)))
           (resultat  (append (quicksort
                               (selecciona-estimacio petits)
                               petits)
                              (cons elemp (quicksort
                                           (selecciona-estimacio grans) grans)))))
      resultat)))

;;;;;;;;;;;;; quicksort ;;;;;;;;;;;;

;;;;;;;;;;;;;   node    ;;;;;;;;;;;;

(defun id (node) (car node))

(defun estat (node) (cadr node))

(defun id-pare (node) (caddr node))

(defun operador (node) (cadddr node))

(defun info (node) (cddddr node))

(defun construeix-node (id estat id-pare op info)
  (append (list id estat id-pare op) info))

(defun expandeix-node (node operadors funcio)
  (labels ((elimina-estats-buits (llista-nodes)
                                 (remove-if #'(lambda (node)
                                                (equal (estat node) 'buit))
                                            llista-nodes)))
    (let ((estat   (estat node))
          (id-node (id node))
          (info    (info node)))
      (elimina-estats-buits
       (mapcar #'(lambda (operador)
                   (construeix-node (gensym)
                                    (funcall (cadr operador) estat info)
                                    id-node
                                    (car operador)
                                    (funcall funcio (list estat info) (funcall (cadr operador) estat info) (car operador))))
               operadors)))))                   ;;; aquí ^ l'original passa només info com a paràmetre

;;;;;;;;;;;;;   node    ;;;;;;;;;;;;

;;;;;;;;;;;;; problema  ;;;;;;;;;;;;

(defun operadors (problema) (car problema))

(defun funcio-info-addicional (problema) (cadr problema))

(defun estat-inicial (problema) (caddr problema))

(defun funcio-objectiu (problema) (cadddr problema))

(defun info-inicial (problema) (car (cddddr problema)))

(defun solucio? (problema node) (funcall (funcio-objectiu problema) (estat node)))

;;;;;;;;;;;;; problema  ;;;;;;;;;;;;

;;;;;;;;;;;;;   arbre   ;;;;;;;;;;;;

(defun nodes-a-expandir (arbre)
  (car arbre))

(defun nodes-expandits (arbre)
  (cadr arbre))

(defun selecciona-node (arbre)
  (car (nodes-a-expandir arbre)))

(defun candidats? (arbre)
  (not (null (nodes-a-expandir arbre))))

(defun cami (arbre node)
  (if (null (id-pare node)) nil
    (append (cami arbre
                  (node-arbre (id-pare node) arbre))
            (list (operador node)))))

(defun node-arbre (id-node arbre)
  (let ((a-expandir? (member-if #'(lambda (node) (equal (id node) id-node)) (nodes-a-expandir arbre))))
    (if a-expandir?
        (find-if #'(lambda (node) (equal (id node) id-node))
                 (nodes-a-expandir arbre))
      (find-if #'(lambda (node) (equal (id node) id-node))
               (nodes-expandits arbre)))))

(defun construeix-arbre (arbre estrategia node-expandit nous-nodes-a-expandir)
  (cons (funcall estrategia (car arbre) nous-nodes-a-expandir)
        (list (cons node-expandit (cadr arbre)))))

(defun expandeix-arbre (problema estrategia arbre node)
  (let ((nous-nodes-a-expandir (expandeix-node node
                                               (operadors problema)
                                               (funcio-info-addicional problema)) ))
    (construeix-arbre arbre estrategia node nous-nodes-a-expandir)))

(defun elimina-seleccio (arbre)
  (cons (cdr (nodes-a-expandir arbre))
        (cdr arbre)))

(defun arbre-inicial (estat info)
  (list (list (append (construeix-node (gensym) estat nil nil nil) (funcall info estat)))))

;;;;;;;;;;;;;   arbre   ;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cerca (problema estrategia arbre)
  (if (not (candidats? arbre)) '(no-hi-ha-solucio)
    (let ((node (selecciona-node arbre))
          (nou-arbre (elimina-seleccio arbre)))
      (if (solucio? problema node)
          (cami arbre node)
        (cerca problema estrategia (expandeix-arbre problema estrategia nou-arbre node))))))

(defun fer-cerca (problema estrategia)
  (cerca problema estrategia (arbre-inicial (estat-inicial problema)
                                            (info-inicial problema))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;; cerca A*

;;;;;;;;; Definició del graf:
;;;;;;;;;
;;;;;;;;; Un operador per a cada aresta del graf

(defun AtoB (estat info)
  (if (equal estat 'A) 'B 'buit))
(defun AtoC (estat info)
  (if (equal estat 'A) 'C 'buit))
(defun AtoD (estat info)
  (if (equal estat 'A) 'D 'buit))
(defun BtoD (estat info)
  (if (equal estat 'B) 'D 'buit))
(defun BtoG (estat info)
  (if (equal estat 'B) 'G 'buit))
(defun CtoB (estat info)
  (if (equal estat 'C) 'B 'buit))
(defun CtoE (estat info)
  (if (equal estat 'C) 'E 'buit))
(defun DtoE (estat info)
  (if (equal estat 'D) 'E 'buit))
(defun EtoF (estat info)
  (if (equal estat 'E) 'F 'buit))
(defun EtoH (estat info)
  (if (equal estat 'E) 'H 'buit))
(defun FtoG (estat info)
  (if (equal estat 'F) 'G 'buit))
(defun GtoH (estat info)
  (if (equal estat 'G) 'H 'buit))
(defun HtoI (estat info)
  (if (equal estat 'H) 'I 'buit))
(defun ItoC (estat info)
  (if (equal estat 'I) 'C 'buit))

(defvar tl-operadors
  (list (list 'AtoB #'AtoB)
        (list 'AtoC #'AtoC)
        (list 'AtoD #'AtoD)
        (list 'BtoD #'BtoD)
        (list 'BtoG #'BtoG)
        (list 'CtoB #'CtoB)
        (list 'CtoE #'CtoE)
        (list 'DtoE #'DtoE)
        (list 'EtoF #'EtoF)
        (list 'EtoH #'EtoH)
        (list 'FtoG #'FtoG)
        (list 'GtoH #'GtoH)
        (list 'HtoI #'HtoI)
        (list 'ItoC #'ItoC)))

;;;;;;;;; Funció heurística -- apartat 2-a PAC2 2017-18-Q1

(defun heuristica (estat)
  "Retornar el valor definit a la taula superior depenen de l'entrada."
  (cond
    ((equal estat 'A) 15)
    ((equal estat 'B) 2)
    ((equal estat 'C) 15)
    ((equal estat 'D) 18)
    ((equal estat 'E) 7)
    ((equal estat 'F) 5)
    ((equal estat 'G) 0)
    ((equal estat 'H) 9)
    ((equal estat 'I) 2)
    (t 1000)))

;;;;;;;;; Funció de cost -- apartat 2-b PAC2 2017-18-Q1

(setq mapvector '((A B 20) (A C 20) (A D 1)
                  (B D 4) (B G 4)
                  (C B 3) (C E 3)
                  (D E 5)
                  (E F 9) (E H 2)
                  (F G 10)
                  (G H 4)
                  (H I 1)))

(defun cost (estat1 estat2)
  "Retorna el cost entre dos estats que estiguin connectats en el graf de la part 1"
  (cerca-vector estat1 estat2 mapvector))

(defun cerca-vector (estat1 estat2 mapa)
  (if (null mapa) 1000
    (if (and
         (equal (caar mapa) estat1)
         (equal (cadar mapa) estat2))
      (caddar mapa)
      (cerca-vector estat1 estat2 (cdr mapa)))))


;;;;;;;;; ALgorismes generals cerca A*

(defun tl-estrategia-A* (nodes-a-expandir nous-nodes-a-expandir)
  (let ((unio-nodes (append nous-nodes-a-expandir
                            nodes-a-expandir)))
    (quicksort (selecciona-estimacio unio-nodes)
               unio-nodes)))

(defvar problema-cercaA*
  (list tl-operadors
        #'(lambda (info-node-pare estat nom-operador) ;;; *** info-node-pare = (estat-pare (g g-plus-h))
            (let ((estat-pare  (car info-node-pare))  ;;; Això també és diferent de l'original
                  (g           (caadr info-node-pare))
                  (g-plus-h    (cadadr info-node-pare)))
              (list (+ g (cost estat-pare estat))
                    (+ (+ g (cost estat-pare estat)) (heuristica estat)))))
        'A
        #'(lambda (estat) (equal estat 'G))
        #'(lambda (estat) (list 0 (heuristica estat))) ))

(defun cerca-A* (problema)
  (fer-cerca problema #'tl-estrategia-A*))
