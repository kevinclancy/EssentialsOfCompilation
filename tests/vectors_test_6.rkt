(let ([v (vector 1 2)])
  (let ([w (vector-set! v 0 3)])
    (begin
      (vector-set! v 0 42)
      (vector-ref v 0))))
