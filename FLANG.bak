#lang pl



(define-type Animal
  [Snake Symbol Number Symbol]
  [Tiger Symbol Number]
  )



(define miz (Tiger 'miz 6))



(define-type WAE
  [Num Number]
  [Add WAE WAE]
  [Sub WAE WAE]
  [Id Symbol]
  [With Symbol WAE WAE]
  [Mul WAE WAE]
  [Div WAE WAE]
  )

  


(: parse-expr : Sexpr -> WAE)
(define (parse-expr exp)
  (match exp
        [(number: exp) (Num exp)]
        [(symbol: name) (Id name)]
        [(list '+ l r) (Add (parse-expr l) (parse-expr r))]
        [(list '- l r)(Sub (parse-expr l) (parse-expr r))]
        [(list '* l r) (Mul (parse-expr l) (parse-expr r))]
        [(list '/ l r) (Div (parse-expr l) (parse-expr r))]
        [(cons 'with _ )
         (match exp
       [(list 'with (list (symbol: name ) named-expr) body)
        (With name (parse-expr named-expr) (parse-expr body))
        ]
       [else (error 'parse-expr "bad syntax with in ~s" exp) ]

       )

     ]
        [else (error 'parse-sexpr "bad syntax in ~s" exp)]
        )
  )







(test (parse-expr (string->sexpr "{with {x {+ 4 2}} {+ x x }}"))=> (With 'x (Add (Num 4) (Num 2)) (Add (Id 'x) (Id 'x))))



(: parse : String -> WAE)
(define (parse code)
  (parse-expr (string->sexpr code))
)




(: subst : WAE Symbol WAE -> WAE)
(define (subst expr from to)
  (cases expr
    [(Num n) (Num n)]
    [(Add l r) (Add (subst l from to) (subst r from to))]
    [(Sub l r) (Sub (subst l from to) (subst r from to))]
    [(Mul l r) (Mul (subst l from to) (subst r from to))]
    [(Div l r) (Div (subst l from to) (subst r from to))]
    [(Id name) (if (equal? name from) to expr)]
    [(With name named-expr body)
     (With name (subst named-expr from to)
           (if (equal? from name)
               body
               (subst body from to)
               )
           )]

    ))


(: eval : WAE -> Number)
(define (eval expr)
(cases expr
[(Num n) n]
[(Add l r) (+ (eval l) (eval r))]
[(Sub l r) (- (eval l) (eval r))]
[(Mul l r) (* (eval l) (eval r))]
[(Div l r) (/ (eval l) (eval r))]
[(Id name) (error 'eval "free identifier ~s" name)]
[(With name named-expr body)
(eval (subst body name (Num (eval named-expr))))]
 ))
  

(: run : String -> Number)
(define (run expr)
  (eval (parse expr))
  )

(test (run "5") => 5)
(test (run "z") =error> "free ")
(test (run "{+ 5 5}") => 10)
(test (run "{with {x {+ 5 5}} {+ x x}}") => 20)
(test (run "{with {x 5} {+ x x}}") => 10)
(test (run "{with {x {+ 5 5}}
{with {y {- x 3}}
{+ x y}}}") => 17)
(test (run "{with {x 5}
{+ x {with {x 3} 10}}}") => 15)
(test (run "{with {x 5}
{+ x {with {x 3} {+ x x}}}}") => 11)
(test (run "{with {x {+ 3 4}} {+ x {with {y 3} x}}}") => 14)
(test (run "{with {x 5}
{with {y {* x 3}}
{- y x}}}") => 10)
(test (run "{with {x 8}
{with {x {* x x}}
{/ x 4}}}") => 16)


#|


(: eval : WAE -> Number)
(define (eval expr)
(cases expr
[(Num n) n]
[(Add l r) (+ (eval l) (eval r))]
[(Sub l r) (- (eval l) (eval r))]
[(Mul l r) (* (eval l) (eval r))]
[(Div l r) (/ (eval l) (eval r))]
[(Id name) (error 'eval "free identifier ~s" name)]
[(With name named-expr body)
(eval (subst body name (Num (eval named-expr))))]
 )





(: run : String -> Number)
(define (run expr)
  (eval (parse expr))
  )


(test (run "3") => 3)
(test (run "{ + 3 4 }") => 7)
(test (run "{ - { + 3 4 } 6 }") => 1)




|#










