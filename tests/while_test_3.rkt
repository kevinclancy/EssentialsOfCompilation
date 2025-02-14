(let ([x 0])
  (let ([y 3])
    (begin
      (set! x 2)
      (if (or (>= (+ x y) 4) #f) 42 1))))
