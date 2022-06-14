#lang pl 02

;;Q1

#|
 The LE grammer
 we created the LE grammer so that The valid functions that can be used in these expressions are cons,
 list,append, and null as requierd.
 * The grammar allows cons with an expression that represents a list as the second part of a cons expression.
 * List can have any number of arguments (including zero arguments)
 * Append can have any number of list arguments

 
|#


#|

----A------

The LE grammer

 1. <LE> ::=
           | { cons <A> <B> } |1.1
           | { list <LE>(...) } |1.2
           | { append <Append> <Append>(...) } |1.3
           | '<sym> |1.4
           | <null> |1.5
           | <num> |1.6

 2. <Append> ::= | { cons <A> <B> } |2.1
                 | { list <LE>(...) } |2.2
                 | <null> |2.3
                 | { append <Append> <Append>(...) } |2.4


 3. <A> ::= | '<sym> |3.1
            | <null> |3.2
            | <num> |3.3
            | { cons <A> <B> } |3.4
            | { append <Append> <Append>(...) } |3.5


 4. <B> ::= | { cons <A> <B> } |4.1
            |  <null> |4.2
            | { list <LE>(...) } |4.3
            | { append <Append> <Append>(...) } |4.4

------D------
3 exapmle of derrived word 

a) (cons 316 (cons (append (cons 'rivka null) (list 'st 'r )) null))
  
=>(1.1)
(cons <A> <B>)=>(3.3)
(cons 316 <B>)=>(4.1)
(cons 316 (cons <A> <B> ))=>(3.5)
(cons 316 (cons (append <Append> <Append>) <B> )) =>(4.2)
(cons 316 (cons (append <Append> <Append>) null ))=>(2.1)
(cons 316 (cons (append (cons <A> <B> ) <Append>) null ))=>(3.1)
(cons 316 (cons (append (cons 'rivka <B> ) <Append>) null )) =>(4.2)
(cons 316 (cons (append (cons 'rivka null ) <Append>) null ))=>(2.2)
(cons 316 (cons (append (cons 'rivka null ) (list <LE> <LE>)) null ))=>(1.4)
(cons 316 (cons (append (cons 'rivka null ) (list 'st <LE>)) null ))=>(1.4)
(cons 316 (cons (append (cons 'rivka null ) (list 'st 'r)) null ))



b) (append (list 2 0) (list 7 9) (cons 1 null))

=>(1.3)
(append <Append> <Append> <Append>) =>(2.2)
(append (list <LE> <LE>) <Append> <Append>)=>(2.2)
(append (list <LE> <LE>) (list <LE> <LE>) <Append>) =>(2.1)
(append (list <LE> <LE>) (list <LE> <LE>) (cons <A> <B>)) => (1.6 (*4 times))
(append (list 2 0) (list 7 9) (cons <A> <B>)) =>(3.3)
(append (list 2 0) (list 7 9) (cons 1 <B>))=>(4.2)
(append (list 2 0) (list 7 9) (cons 1 null)) 


c) (cons 1 (cons 'mor (cons 'ia null)))

=>(1.1)
(cons <A> <B>)=>(3.3)
(cons 1 <B>)=>(4.1)
(cons 1 (cons <A> <B>))=>(3.1)
(cons 1 (cons 'mor <B>))=>(4.1)
(cons 1 (cons 'mor (cons <A> <B>)))=>(3.1)
(cons 1 (cons 'mor (cons 'ia <B>)))=>(4.2)
(cons 1 (cons 'mor (cons 'ia null)))

   
             
|#




;;Q2

#|
 we changed the  parse-sexpr to eccept infix gremmer by moving the + - ... to the middle of the pattern.
 and the eval to accept zero division (if divided by zero return inf =999).
 there wasnt much of a difficulty in this Q because we new the code from class and it was very clear what shuld be done
 
|#

#|
The AE grammer

  <AE> ::= <num>
           | { <AE> + <AE> }
           | { <AE> - <AE> }
           | { <AE> * <AE> }
           | { <AE> / <AE> }
|#
(define-type AE
  [Num Number]
  [Add AE AE]
  [Sub AE AE]
  [Mul AE AE]
  [Div AE AE])


(: parse-sexpr : Sexpr -> AE)
(define (parse-sexpr sxp)
  (match sxp
    [(number: n) (Num n)]
    [(list  l '+ r) (Add (parse-sexpr l)(parse-sexpr r))]
    [(list  l '- r) (Sub (parse-sexpr l)(parse-sexpr r))]
    [(list  l '* r) (Mul (parse-sexpr l)(parse-sexpr r))]
    [(list  l '/ r) (Div (parse-sexpr l)(parse-sexpr r))]
    [else (error 'parse-sexpr "bad syntax in ~s" sxp)]))


(: parse : String -> AE)
(define (parse code)
  (parse-sexpr (string->sexpr code)))


;assuming we chose prefix form grammer with curly parentheses
(test (parse "{3 + 4 }") => (Add (Num 3)
                                   (Num 4)))
(test (parse "3") => (Num 3))
(test (parse "{{3 - 2} +  4 }") => (Add (Sub (Num 3)
                                              (Num 2))
                                         (Num 4)))
(test (parse "{+ 1 2 3 4}") =error> "bad syntax")




#|
The goal of parse:
Input:  string describing the program
Output: Abstract Syntax Tree (or an exception if the string is not a valid program)

Two main phases:
1. Read -- turn the string into a simple data structure (we will use the Racket type Sexpr).
2. Actual Parsing -- turn an Sexpr into an AST


Definition of the pl type Sexpr:
Basis -- any Number/Symbol is an Sexpr
General -- any list of Sexpr is an Sexpr

|#
 


#|
;;; ====== EVAL  ==============
; <AE> ::= <num>               a 
;          | { + <AE> <AE> }   b
;          | { - <AE> <AE> }   c

eval(<num>) = <num>
eval({+ E1 E2}) =  eval(E1) + eval(E2)
eval({- E1 E2}) =  eval(E1) - eval(E2)
|#



(: eval : AE -> Number)
(define (eval exp)
  (cases exp
    [(Num n) n]
    [(Add l r) (+ (eval l) (eval r))]
    [(Sub l r) (- (eval l) (eval r))]
    [(Mul l r) (* (eval l) (eval r))]
    [(Div l r) (if (zero? (eval r)) 999 (/ (eval l) (eval r)))]
    ))
    


(: run : String -> Number)
(define (run code)
  (eval (parse code)))


;;Q2 tests
(test (eval (Num 3)) => 3)
(test (eval (Add (Num 3) (Num 4))) => 7)
(test (eval (Add (Sub (Num 3) (Num 2)) (Num 4))) => 5)

(test (eval (parse "{ 3 + 4 }")) => 7)
(test (eval (parse "3")) => 3)
(test (eval (parse "{{ 3 - 2} + 4 }")) => 5)
(test (eval (parse "{+ 1 2 3 4}")) =error> "bad syntax")

(test (eval (parse "{3 * {5 / 3} }")) => 5)
(test (run "{ 3 + 4 }") => 7)
(test (run "3") => 3)
(test (run "{{3 - 2} + 4 }") => 5)
(test (run "{3 / 0}") => 999)
(test (run "{{3 / 0} + 3}") => 1002)
(test (run "{{3 / 0} - 9}") => 990)
(test (run "{+ 1 2 3 4}") =error> "bad syntax")
(test (run "{* 1 2 3 4}") =error> "bad syntax")



;;Q3

#|
 square get a number and returning the square of this number.
 sum-of-squares get a list of number and returning the sum of square on each num of the list
 we used map to call square on each num of the list and then we sum the number using foldl
 *difficulties - understand how foldle work we solved it by reading the decomentation at the end of the task file.
 
|#

(: square : Number -> Number )
(define (square num ) (* num num) ) 


( : sum-of-squares : (Listof Number) -> Number )
(define (sum-of-squares ls)
  (foldl  + 0 (map square ls))
 )

;;Q3 tests
(test (sum-of-squares '(1 2 3)) => 14)
(test (sum-of-squares '()) => 0) ;;empty list returning the init value 
(test (sum-of-squares '(5)) => 25)
(test (sum-of-squares '(3 3 3 3)) => 36)
(test (sum-of-squares '(1 1 1 1 1)) => 5)

#|
  we have created BINTREE data-type with two variant - Node and Leaf.
  the node variant have two BINTREE and the leaf have a number
  tree-map
  input: a function and a BINTREE
  output: a BINTREE with the same structure like the input tree after activating f on every  values of its leaves
 *difficulties - finding the right syntax for higher-order functions.
 **solution - for the syntax we used google. 
 
|#

;;Q4
(define-type BINTREE
[Node BINTREE BINTREE]
[Leaf Number])

(: tree-map : (Number -> Number) BINTREE -> BINTREE)
(define (tree-map f bt)
  (cases bt
    [(Leaf num) (Leaf(f num)) ]
    [(Node bt1 bt2) (Node (tree-map f bt1) (tree-map f bt2))]
     
    )
  )

;;Q4 tests

(test (tree-map add1 (Node (Leaf 1) (Node (Leaf 2) (Leaf
3))))
 => (Node (Leaf 2) (Node (Leaf 3) (Leaf 4))))

(test (tree-map sub1 (Node (Leaf 1) (Node (Leaf 2) (Leaf
3))))
 => (Node (Leaf 0) (Node (Leaf 1) (Leaf 2))))

(test (tree-map square (Node (Leaf 1) (Node (Leaf 2) (Leaf
3))))
 => (Node (Leaf 1) (Node (Leaf 4) (Leaf 9))))

(test (tree-map square (Node (Node (Leaf 1) (Node (Leaf 2)  (Leaf
3)))(Leaf 2) ))
 => (Node (Node (Leaf 1) (Node (Leaf 4)  (Leaf 9)))(Leaf 4) ))

(test (tree-map add1 (Leaf 2) )
 =>  (Leaf 3)) 





