(let ([x 1])
  (if (begin (set! x 5) (> x 2))
   42
   24))
