#lang pl 

;;----------Q1---------------


#|
The AE grammer

  <AE> ::= <num>
           | { <AE> <AE> + }
           | { <AE> <AE> - }
           | { <AE> <AE> * }
           | { <AE> <AE> / }
           | {<AE> <AE> power}
           | {<AE> <AE> sqr}
|#

(define-type AE
  [Num Number]
  [Add AE AE]
  [Sub AE AE]
  [Mul AE AE]
  [Div AE AE]
  [power AE AE]
  [sqr AE ]
  )


(: parse-sexpr : Sexpr -> AE)
(define (parse-sexpr sxp)
  (match sxp
    [(number: n) (Num n)]
    [(list  l r '+) (Add (parse-sexpr l)(parse-sexpr r))]
    [(list  l r '-) (Sub (parse-sexpr l)(parse-sexpr r))]
    [(list  l r '*) (Mul (parse-sexpr l)(parse-sexpr r))]
    [(list  l r '/) (Div (parse-sexpr l)(parse-sexpr r))]
    [(list  l r 'power)  (power (parse-sexpr l) (parse-sexpr r))  ]
    [(list  l 'sqr  ) (sqr (parse-sexpr l))]
    [else (error 'parse-sexpr "bad syntax in ~s" sxp)]))


(: parse : String -> AE)
(define (parse code)
  (parse-sexpr (string->sexpr code)))

(: pow : Number Number -> Number)
(define (pow x y)
  (: helper : Number Number Number -> Number)
   (define (helper x p ans)
     (cond
        [(zero? p) ans]
        [else (helper x (- p 1) (* ans x))] 
        )
     ) 
  (helper x y 1)
    )

(: square : Number -> Number)
(define (square x )
 (* x x ) )


(: eval : AE -> Number)
(define (eval exp)
  (cases exp
    [(Num n) n]
    [(Add l r) (+ (eval l) (eval r))]
    [(Sub l r) (- (eval l) (eval r))]
    [(Mul l r) (* (eval l) (eval r))]
    [(Div l r) (/ (eval l) (eval r))]
    [(power l r) (if (integer? (eval  r))(pow  (eval l) (eval r)) (error 'eval "eval: power expects an integer power, got ~s " exp))]
    [(sqr x )(square  (eval x))]
))
      

(: run : String -> Number)
(define (run code)
  (eval (parse code)))


(test (parse "{ 3  4 +}") => (Add (Num 3)
                                   (Num 4)))
(test (parse "3") => (Num 3))
(test (parse "{ {3 2 -}  4  +}") => (Add (Sub (Num 3)
                                              (Num 2))
                                         (Num 4)))
(test (parse "{ 1 2 3 4 +}") =error> "bad syntax")
(test (parse "{+ 1 2}") =error> "bad syntax")
(test (eval (Num 3)) => 3)
(test (eval (Add (Num 3) (Num 4))) => 7)
(test (eval (Add (Sub (Num 3) (Num 2)) (Num 4))) => 5)

(test (eval (parse "{  3  4 +}")) => 7)
(test (eval (parse "3")) => 3)
(test (eval (parse "{{ 3 2 -}  4  + }")) => 5)
(test (eval (parse "{+ 1 2 3 4}")) =error> "bad syntax")

(test (eval (parse "{ 3  { 5 3 /} *}")) => 5)
(test (run "{  3  4 +}") => 7)
(test (run "3") => 3)
(test (run "{ { 3 2 -}  4  +}") => 5)
(test (run "{+ 1 2 3 4}") =error> "bad syntax")
 
(test (run "3") => 3) 
 (test (run "{3 4 +}") => 7) 
 (test (run "{{3 4 -} 7 +}") => 6) 
 (test (run "{{3 4 power} 7 +}") => 88) 
 (test (run "{{2 4 power} {5 sqr} +}") => 41)
(test (run "{{2 4/4 power} {5 sqr} +}") => 27) ;; power with integer 4/4
(test (run "{{2 4/5 power} {5 sqr} +}") 
 =error> "eval: power expects an integer power, got")
(test (run "{2 85/100 power} ")   
 =error> "eval: power expects an integer power, got")


;;----------Q2--------------- 



;; LE abstract syntax trees 
(define-type LE = (U LIST ATOM)) 
;; LIST abstract syntax trees 
(define-type LIST 
  [Lists (Listof LE)]
  [Append (Listof LIST)]
  [Cons LE LIST]
  [Nul]) 
;; ATOM abstract syntax trees 
(define-type ATOM 
  [Numb Number]
  [Sym Symbol])


(: parse-sexpr->LEs : (Listof Sexpr) -> (Listof LE)) 
 ;; converts a list of s-expressions into a list of LEs 
 (define (parse-sexpr->LEs sexprs)
   (map parse-sexprLE sexprs))


(: parse-sexpr->LISTs : (Listof Sexpr) -> (Listof LIST)) 
;; converts a list of s-exprs into a list of LISTs 
 (define (parse-sexpr->LISTs sexprs) 
 (map parse-sexpr->LIST sexprs))


(: parse-sexpr->LIST : Sexpr -> LIST)
;; to convert s-expressions into List
 (define (parse-sexpr->LIST sexpr)
 (let ([ast (parse-sexprLE sexpr)]) 
 (if (LIST? ast) ast (error 'parse-sexprLE "parsesexprLE: expected LIST; got ~s" ast))))


(: parse-sexprLE : Sexpr -> LE) 
;; to convert s-expressions into LEs 
(define (parse-sexprLE sexpr) 
 (match sexpr 
 [(number: n) (Numb n)] 
 ['null (Nul)] 
 [(symbol: s) (Sym s)] 
 [(list 'cons l r) (Cons (parse-sexprLE l) (parse-sexpr->LIST r) ) ] 
 [(cons 'append lst) (Append (parse-sexpr->LISTs lst) )] 
 [(cons 'list lst) (Lists (parse-sexpr->LEs lst))] 
 [else (error 'parse-sexprLE "parsesexprLE: bad syntax in (cons)")]))  
 
(: parseLE : String -> LE) 
 ;; parses a string containing a LE expression to a 
 ;; LE AST 
(define (parseLE str) 
 (parse-sexprLE (string->sexpr str)))




(: eval-append-args : (Listof LE) -> (Listof (Listof Any))) 
 ;; evaluates LE expressions by reducing them to lists 
 (define (eval-append-args exprs) 
 (if (null? exprs) null 
 (let ([fst-val (evalLE (first exprs))]) 
 (if (list? fst-val) 
 (cons fst-val (eval-append-args (rest exprs))) 
 (error 'evalLE "append argument: expected List got ~s" fst-val)))))


 (: evalLE : LE -> Any) 
 ;; evaluates LE expressions by reducing them to numbers 
 (define (evalLE expr) 
 (if (LIST? expr) 
 (cases expr
 [(Nul) null]
 [(Lists lst)(map evalLE lst)] 
 [(Cons l r) (let ([evr (evalLE r)])(if (list? evr) (cons (evalLE l) evr) (error 'evalLE "expected List got ~s" evr) ))] 
 [(Append lst ) (apply append (eval-append-args lst))]) 
 (cases expr 
 [(Numb n) n] 
 [(Sym s) s])))


 (: runLE : String -> Any) 
 ;; evaluate a WAE program contained in a string 
 (define (runLE str) 
 (evalLE (parseLE str)))



(test (parseLE "3") => (Numb 3))
(test (parseLE "hey") => (Sym 'hey))
(test (parseLE "{cons 1 {cons two null}}") =>
(Cons (Numb 1) (Cons (Sym 'two) (Nul))))


#|
(test (parseLE "{append {list {cons {cons 1 null} {cons two null}}}}") =>
(Append (Lists (Cons (Numb 1)(Nul)) (Cons (Sym 'two) (Nul)))))  
|#

(test (runLE "null") => null)
(test (runLE "12") => 12)
(test (runLE "boo") => 'boo)
(test (runLE "{cons 1 {cons two null}}") =>
'(1 two))
(test (runLE "{list 1 2 3}") => '(1 2 3))
(test (runLE "{list 1 {cons 2 {cons 3 null}}}") => '(1 (2 3)) )
(test (runLE "{list {cons}}") =error> "parsesexprLE: bad syntax in (cons)") 
(test (runLE "{list {cons 2 1}}") =error>
"parsesexprLE: expected LIST; got")   
