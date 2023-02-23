#lang pl



(define-type Animal
  [Snake Symbol Number Symbol]
  [Tiger Symbol Number]
  )



(define miz (Tiger 'miz 6))



(define-type FLANG
  [Num Number]
  [Add FLANG FLANG]
  [Sub FLANG FLANG]
  [Id Symbol]
  [With Symbol FLANG FLANG]
  [Mul FLANG FLANG]
  [Div FLANG FLANG]
  [Fun Symbol FLANG]
  [Call FLANG FLANG]
  )

  


(: parse-expr : Sexpr -> FLANG)
(define (parse-expr exp)
  (match exp
        [(number: exp) (Num exp)]
        [(symbol: name) (Id name)]
        [(list '+ l r) (Add (parse-expr l) (parse-expr r))]
        [(list '- l r)(Sub (parse-expr l) (parse-expr r))]
        [(list '* l r) (Mul (parse-expr l) (parse-expr r))]
        [(list '/ l r) (Div (parse-expr l) (parse-expr r))]
    [(cons 'fun more)
     (match exp
       [(list 'fun (list (symbol: name )) body)
       (Fun name (parse-expr body))]
       [else (error 'parse-expr "bard 'fun syntax in ~s" exp)]
       

         )

     ]
    [(list 'call fun arg) (Call (parse-expr fun) (parse-expr arg))]

    
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



(: parse : String -> FLANG)
(define (parse code)
  (parse-expr (string->sexpr code))
)




(: subst : FLANG Symbol FLANG -> FLANG)
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
    [(Fun bound-id bound-body)
     (if (eq? bound-id from)
         expr
         (Fun bound-id (subst bound-body from to))
         )
     ]
    [(Call l r) (Call (subst l from to) (subst r from to))]
    

    
    ))



(: arith-op : (Number Number -> Number) FLANG FLANG -> FLANG)
(define (arith-op op expr1 expr2)
  (: Num->number : FLANG -> Number )
  (define (Num->number e)
    (cases e
      [(Num n) n]
      [else (error 'arithop "exprects a number got: ~s" e)]

      )
    )
(Num (op (Num->number expr1) (Num->number expr2)))

  )







(: eval : FLANG -> FLANG)
(define (eval expr)
(cases expr
[(Num n) expr]
[(Add l r) (arith-op + (eval l) (eval r))]
[(Sub l r) (arith-op - (eval l) (eval r))]
[(Mul l r) (arith-op * (eval l) (eval r))]
[(Div l r) (arith-op / (eval l) (eval r))]
[(Id name) (error 'eval "free identifier ~s" name)]
[(With bound-id named-expr bound-body)
(eval (subst bound-body bound-id  (eval named-expr)))]
[(Fun name body) expr]
[(Call fun-e arg-e)
 (let ([funv (eval fun-e)])
   (cases funv
     [(Fun name body)
      (eval (subst body name (eval arg-e)))
      ]
     [else (error 'eval "invalid fun ~s" expr)]
     
     

     )
   


   )

 ]

  ))

  

(: run : String -> Number)
(define (run expr)
  (let ([res (eval (parse expr))])
    (cases res
      [(Num n) n]
      [else (error 'run "not valid res ~s" expr)]
      )
    )
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


(test (run "5") => 5)
(test (run "{+ 4 6}") => 10)
(test (run "{+ 4 6}") => 10)










