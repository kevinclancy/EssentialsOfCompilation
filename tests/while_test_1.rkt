(let ([x 0])
  (begin
   (set! x 2)
   (if (or (>= (+ x 3) 4) #f) 42 1)))
