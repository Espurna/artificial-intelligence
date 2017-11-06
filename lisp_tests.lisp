(defun heuristica2 (x)
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
