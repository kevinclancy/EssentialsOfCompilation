(let ([x 2])
(let ([y (begin (set! x 3) (+ x 1))])
  (+ y 38)))
