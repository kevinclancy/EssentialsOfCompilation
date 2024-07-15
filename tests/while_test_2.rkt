(let ([x #t])
  (begin
   (set! x #f)
   (if (or x (begin (set! x #t) x)) 42 1)))

