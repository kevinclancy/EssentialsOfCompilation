(let ([v (vector 1 2)])
  (begin
    (vector-set! v 0 42)
    (vector-ref v 0)))
