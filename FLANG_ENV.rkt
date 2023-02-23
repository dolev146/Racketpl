#lang pl



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


(define-type SubstCache = (Listof (List Symbol FLANG)))
(: empty-subst : SubstCache)
(define empty-subst null)
(: extend : Symbol FLANG SubstCache -> SubstCache)
(define (extend id expr sc)
  (cons (list id expr) sc)
  )
  


(define-type ENV
  [EmptyEnv]
  [Extend Symbol VAL ENV]
  )


(define-type VAL
  [NumV Number]
  [FunV Symbol FLANG ENV]
  )




(: lookup : Symbol ENV -> VAL)
(define (lookup name env)
  (cases env
    [(EmptyEnv) (error 'lookup "no binding for ~s" name)]
    [(Extend id val rest-env)
     (if (eq? id name) val (lookup name rest-env))

     ]
    

   )
  )




(: arith-op : (Number Number -> Number) VAL VAL -> VAL)
(define (arith-op op expr1 expr2)
  (: Num->number : VAL -> Number )
  (define (Num->number e)
    (cases e
      [(NumV n) n]
      [else (error 'arithop "exprects a number got: ~s" e)]

      )
    )
(NumV (op (Num->number expr1) (Num->number expr2)))
)







(: eval : FLANG ENV -> VAL)
(define (eval expr env)
(cases expr
[(Num n) (NumV n)]
[(Add l r) (arith-op + (eval l env) (eval r env))]
[(Sub l r) (arith-op - (eval l env) (eval r env))]
[(Mul l r) (arith-op * (eval l env) (eval r env))]
[(Div l r) (arith-op / (eval l env) (eval r env))]
[(Id name) (lookup name env)]
[(With bound-id named-expr bound-body)
(eval bound-body (Extend bound-id (eval named-expr env) env))]
[(Fun name body) (FunV name body env)]
[(Call fun-e arg-e)
 (let ([funv (eval fun-e env)])
   (cases funv
     [(FunV name body f-env)
      (eval body (Extend name (eval arg-e env) f-env))
      ]
     [else (error 'eval "invalid fun ~s" expr)]
     
     

     )
   


   )

 ]

  ))

  

(: run : String -> Number)
(define (run expr)
  (let ([res (eval (parse expr) (EmptyEnv) )])
    (cases res
      [(NumV n) n]
      [else (error 'run "not valid res ~s" expr)]
      )
    )
  )



  
  

(test (run "5") => 5)
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










