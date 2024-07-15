(let ([x 2])
  (begin (begin (set! x 42) 0) x))

