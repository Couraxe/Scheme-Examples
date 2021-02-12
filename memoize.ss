;;; We use the memoize procedure from The Scheme Programming Language.
;;; An additional implementation allowing for arbitrary arity > 0 memoization is provided as memoize-n.
;;; Wrapping it around an arbitrary procedure of arity > 0 will cache new arguments and perform a lookup using assq.
;;; The cache is an association list of the pattern ((x . ans) ...)

;;; fib-mem using memoize reaches as low as 0.000024182s elapsed CPU time succeeding a (fib-mem 10) call.

(define memoize
    (lambda (f)
      (let ([cache '()])
        (lambda (x)
          (cond
            [(assq x cache) => cdr]
            [else
             (let ([y (f x)])
               (set! cache (cons (cons x y) cache))
               y)])))))
               
(define memoize-n
    (lambda (f)
      (let ([cache '()])
        (lambda args
          (cond
            [(assq args cache) => cdr]
            [else
             (let ([y (apply f args)])
               (set! cache (cons (cons args y) cache))
               (printf "cache = ~a~%" cache)
               y)])))))

;;; Example application is a memoized Fibonacci function.
(define fib-mem
    (memoize
      (lambda (n)
        (cond
          [(< n 2) n]
          [else
           (+ (fib-mem (- n 1)) (fib-mem (- n 2)))]))))

(display (fib-mem 10))
(newline)
(display (fib-mem 30))
(newline)
(display (fib-mem 100))
(newline)
