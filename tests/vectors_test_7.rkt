(let ([v (vector 20 #t 38 (vector 1 2))])
  (+ (if (vector-ref v 1) 42 1) 0))
