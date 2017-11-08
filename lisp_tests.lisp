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
