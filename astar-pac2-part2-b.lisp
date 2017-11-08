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
                                               (funcio-info-addicional problema))))
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
(defun AtoE (estat info)
  (if (equal estat 'A) 'E 'buit))

(defun BtoF (estat info)
  (if (equal estat 'B) 'F 'buit))
(defun CtoF (estat info)
  (if (equal estat 'C) 'F 'buit))
(defun DtoF (estat info)
  (if (equal estat 'D) 'F 'buit))
(defun EtoF (estat info)
  (if (equal estat 'E) 'F 'buit))

(defun EtoH (estat info)
  (if (equal estat 'E) 'H 'buit))
(defun FtoG (estat info)
  (if (equal estat 'F) 'G 'buit))
(defun FtoH (estat info)
  (if (equal estat 'F) 'H 'buit))
(defun HtoZ (estat info)
  (if (equal estat 'H) 'Z 'buit))

(defvar tl-operadors
  (list (list 'AtoB #'AtoB)
        (list 'AtoC #'AtoC)
        (list 'AtoD #'AtoD)
        (list 'AtoE #'AtoE)
        (list 'BtoF #'BtoF)
        (list 'CtoF #'CtoF)
        (list 'DtoF #'DtoF)
        (list 'EtoF #'EtoF)
        (list 'EtoH #'EtoH)
        (list 'FtoG #'FtoG)
        (list 'FtoH #'FtoH)
        (list 'HtoZ #'Htoz)))

;;;;;;;;; Funció heurística -- apartat 2-a PAC2 2017-18-Q1
;A 80
;B 90
;C 60
;D 65
;E 70
;F 50
;G 50
;H 50
;Z 0
(defun heuristica (x)
  "Retornar el valor definit a la taula superior depenen de l'entrada."
  (cond
    ((equal x "A") 80)
    ((equal x "B") 90)
    ((equal x "C") 60)
    ((equal x "D") 65)
    ((equal x "E") 70)
    ((equal x "F") 50)
    ((equal x "G") 50)
    ((equal x "H") 50)
    ((equal x "Z") 0)
    (t 1000)))

;;;;;;;;; Funció de cost -- apartat 2-b PAC2 2017-18-Q1

(setq map '(("A" "B" 4) ("A" "C" 10) ("A" "D" 10) ("A" "E" 15)
            ("B" "F" 5) ("C" "F" 20) ("D" "F" 10) ("E" "F" 3) ("E" "H" 30)
            ("F" "G" 35) ("F" "H" 30)
            ("H" "Z" 2)))

(defun cost (estat1 estat2)
  "Retorna el cost entre dos estats que estiguin connectats en el graf de la part 1"
  (cerca estat1 estat2 map))

(defun cerca (estat1 estat2 mapa)
  (if (null mapa) nil
    (if (and
         (equal (caar mapa) estat1)
         (equal (cadar mapa) estat2))
      (caddar mapa)
      (cerca estat1 estat2 (cdr mapa)))))

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
        #'(lambda (estat) (equal estat 'Z))
        #'(lambda (estat) (list 0 (heuristica estat)))))

(defun cerca-A* (problema)
  (fer-cerca problema #'tl-estrategia-A*))
