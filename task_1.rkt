#lang pl

#|
 Q1
  input: five numbers
  output -> list of number containing the min and max out of the 5 input numbers
  *At first i insert the 5 num to a list and then iterate over the list using recursion function (maxormin) to find the min and max
  in each iteration i used racket first function to get the first number from the list and then sent the rest of the list to the maxormin again
  **difficulties + solving : my main difficulty in this Q was to get use to the syntax and to anderstand how to access the element of the list 
|#

(: min&max : Number Number Number Number Number ->(Listof Number))
(define (min&max x1 x2 x3 x4 x5)
  
    (: maxormin : (Listof Number) Number Number ->(Listof Number))
    (define (maxormin ls min max)
      (cond
        [(null? ls) (list min max)]
        [(> (first ls) max) (maxormin (rest ls) min (first ls))]
        [(< (first ls) min) (maxormin (rest ls) (first ls) max)]
        [else ( maxormin (rest ls) min max )]
        )
    )
   (maxormin (list x1 x2 x3 x4 x5) x1 x1)   
)


;; Q1 tests 

(test (min&max 2 3 2 7 5) => '(2 7))
(test (min&max 0 0 0 0 0) => '(0 0))
(test (min&max 10 21 30 300 7) => '(7 300))
(test (min&max -2 -5 -6 -7 -19) => '(-19 -2)) ;;only negative
(test (min&max -2 15 6 -7 -19) => '(-19 15)) ;; negative and positive numbers


#|
Q2.a
 input: Listof Any
 output -> Listof Number containing only the numbers from the input list.
 *I used a helper function thst gets as input Listof Any and Listof Number and returning Listof Number. the function iterates over the input list(Listof Any) and inserting to
 the Listof Number only the element wich are numbers.
 **difficulties + solving : I culd not append number to list of number so i had to make a new list and then append the two .
|#

(: sublist-numbers : (Listof Any) ->(Listof Number))
(define (sublist-numbers ls)
  
  (: helper : (Listof Any)(Listof Number) ->(Listof Number))
  (define (helper lsA lsN)
    (cond
     [(null? lsA) lsN]
     [(number? (first lsA)) (helper (rest lsA) (append (list (first lsA)) lsN ))]
     [else (helper (rest lsA) lsN)]
     )
    )
  (helper ls (list))
)

;; Q2.a tests
(test (sublist-numbers (list 'any "Benny" 10 'OP 8))
 => '(8 10))
(test (sublist-numbers '(any "Benny" OP (2 3)))
 => null)
(test (sublist-numbers '(any "Benny" OP s #f ))
 => null)
(test (sublist-numbers '()) ;emty list
 => null)
(test (sublist-numbers '(any "Benny" OP s #f 3 4 5 6 ))
 => `(6 5 4 3))
(test (sublist-numbers '(-3 4 5 "100" 6 "89")) ;negaetive num
 => `(6 5 4 -3))

#|
Q2.b
 input:Listof(Listof Any)
 output -> Listof(Listof Number) returning a list that contain lists of the min and max fron each subNumber list 
 *I used a recursion helper function that gets Listof(Listof Any) and Listof(Listof Number) and returning Listof(Listof Number).
 I used the sublist-numbers function from Q2.a as is and changed the min&max function from Q1 so the it will get listof numbers as input insted of 5 num only.
 I built the final  output list with cons and sent it back to the helper function with the rest of the input list (of any).
 **difficulties + solving: at first i sent to min&max-lists an empty list of list like that -(list (list))
 and then because i used cons to make the output list (newLs) list i had 2 emty sub lists in the output list.
 I tryed to solve it by returning the (rest) of the list and it did worked but it was a workaround and not a real solution
 After some trying i sent `() to the functio and it worked :)
 
 

|#
(: min&max-lists : (Listof(Listof Any)) -> (Listof(Listof Number)) )
(define (min&max-lists ls)
  
  (: helper : (Listof(Listof Any)) (Listof(Listof Number)) -> (Listof(Listof Number)))
  (define (helper lsA newLs)
    (cond
        [(null? lsA) (reverse newLs)]
        [ else (helper (rest lsA) (cons (min&max2b (sublist-numbers (first lsA ))) newLs)) ]  
     )
   )
  (helper ls `())
)


(: min&max2b :(Listof Number) ->(Listof Number))
(define (min&max2b lsN)
 
    (: maxormin : (Listof Number) Number Number ->(Listof Number))
    (define (maxormin ls min max)
      (cond
        [(null? ls) (list min max)]
        [(> (first ls) max) (maxormin (rest ls) min (first ls))]
        [(< (first ls) min) (maxormin (rest ls) (first ls) max)]
        [else ( maxormin (rest ls) min max )]
        )
    )
  (if (eq? lsN null) null (maxormin lsN (first lsN) (first lsN)) )
      
)

;; Q2.b tests

(test (min&max-lists '((any "Benny" 10 OP 8) (any "Benny" OP (2 
3)))) => '((8 10) ()))
(test (min&max-lists '((2 5 1 5 L) (4 5 6 7 3 2 1) ())) 
 => '((1 5) (1 7) ()))
(test (min&max-lists '(() () ())) 
 => '(() () ()))
(test (min&max-lists '()) 
 => '())
(test (min&max-lists '(( "hh" L) () (H J K "II" ))) 
 => '(() () ()))
(test (min&max-lists '(( "hh" L 1 4) () (H J K "II" 67 -1 9 -15))) 
 => '((1 4) () (-15 67)))
(test (min&max-lists '(( "hh" L 1 4) () (H J K "II" (67 -1 9 -15) ))) 
 => '((1 4) () ()))

#|
Q3

 I have created KeyStack data-type with two variant - EmptyKS and Push.

 search-stack
 input:Symbol and KeyStack
 output-> string if the wanted element existes and false otherwise
 *I used cases to match the patterns of the two variant of the KeyStack data 
 type.(EmptyKS and Push). if the curr symbol in the KeyStack equal to the wantes sybol i returned the string and if not i sent the rest of the KeyStack back to the function
 **difficulties + solving: my difficulty was to andersted that the KeyStack that we get in the push varient is the rest of the KeyStack (like a pointer to the next element) once i realized that
 it was clear to me what to do and the quastion wad rather easy.

 pop-stack
 input: KeyStack
 output-> KeyStack without the top value
 *I used cases to match the patterns of the two variant of the KeyStack data 
 type.(EmptyKS and Push). if the stack is empty return false and if not all we need to do is to return the rest of the KeyStack
 from the push varient.
 **difficulties + solving:i had no difficulty in this part of the question
|#


(define-type  KeyStack
[EmptyKS]
[Push Symbol String  KeyStack])


( : search-stack : Symbol KeyStack -> (U String #f))
(define (search-stack symbol stack )
  (cases stack
    [(EmptyKS)  #f]
    [(Push sy st ks) (if (eq? sy symbol) st (search-stack symbol ks))]
    
    )
)


( : pop-stack : KeyStack -> (U KeyStack #f))
(define (pop-stack stack)
  
  (cases stack
    [(EmptyKS)  #f]
    [(Push  sy  st ks) ks]
     
    )
 )

;;Q3 tests

(test (EmptyKS) => (EmptyKS))
(test (Push 'b "B" (Push 'a "A" (EmptyKS))) =>
 (Push 'b "B" (Push 'a "A" (EmptyKS))))
(test (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS)))) => 
(Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS)))))
(test (Push 'two "222"(Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS))))) => 
(Push 'two "222"(Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS))))))
(test (search-stack 'a (Push 'a "AAA" (Push 'b "B" (Push 'a "A" 
(EmptyKS))))) => "AAA")

(test (search-stack 'b (Push 'two "222"(Push 'a "AAA" (Push 'b "B" (Push 'a "A" 
(EmptyKS)))))) => "B")
(test (search-stack 'b (Push 'a "AAA"(Push 'a "AAA" (Push 'a "AAA" (Push 'a "AAAA" 
(EmptyKS)))))) => #f) ;not exist
(test (search-stack 'a (Push 'a "AAAaa"(Push 'a "AAA"(Push 'a "AAA"(Push 'a "AAA" (Push 'a "AAA" (Push 'a "AAAA" 
(EmptyKS)))))))) => "AAAaa") ;identical symbole
(test (search-stack 'c (Push 'a "AAA" (Push 'b "B" (Push 'a "A" 
(EmptyKS))))) => #f)
(test (search-stack 'c (EmptyKS)) => #f) ;search-stack in empty stack

(test (pop-stack (Push 'a "AAA" (Push 'b "B" (Push 'a "A" 
(EmptyKS))))) => (Push 'b "B" (Push 'a "A" (EmptyKS))))
(test (pop-stack (Push 'a "AAA"(Push 'a "AAA" (Push 'b "B" (Push 'a "A" 
(EmptyKS)))))) => (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS)))))
(test (pop-stack (EmptyKS)) => #f) ;pop to empty stack


(test (min&max 5 4 3 2 1) => '(1 5))
(test (min&max 5 6 3 -5 -9) => '(-9 6))
(test (min&max 0 0 0 0 0) => '(0 0))
(test (min&max -5 -2 -7 -3 -3) => '(-7 -2))
(test (or (equal? (min&max -5 -2.5 -7 -3 -3) '(-7 -2.5)) (equal? (min&max -5 -2.5 -7 -3 -3) '(-7.0 -2.5))) => #t) 
(test (or (equal? (min&max 1.5 8 3.3 1 0.2) '(0.2 8)) (equal? (min&max 1.5 8 3.3 1 0.2) '(0.2 8.0))) => #t)
(test (or (equal? (sublist-numbers (list 'adf "moshe" 0 20 'ds)) '(0 20)) (equal? (sublist-numbers (list 'adf "moshe" 0 20 'ds)) '(20 0))) => #t) 
(test (sublist-numbers '(any "Benny" OP (2 3))) => null)
(test (sublist-numbers '()) => null)
(test (or (equal? (sublist-numbers '(1 2 3 4 5 6)) '(6 5 4 3 2 1)) (equal? (sublist-numbers '(1 2 3 4 5 6)) '(1 2 3 4 5 6))) => #t) 
(test (or (equal? (sublist-numbers '(1.5 8 3.3 1 0.2)) '(1.5 8 3.3 1 0.2)) (equal? (sublist-numbers '(1.5 8 3.3 1 0.2)) '(0.2 1 3.3 8 1.5))) => #t) 
(test (sublist-numbers '(4 (fd)'((1 2 3)))) => '(4))
(test (sublist-numbers '(any "Benny" OP (2 3))) => '())

(test (or (equal? (sublist-numbers '(any "Benny" OP 2 3)) '(2 3)) (equal? (sublist-numbers '(any "Benny" OP 2 3)) '(3 2))) => #t)

(test (min&max-lists '((2 5 1 5 L) () (4 5 6 7.8 3 2 1) ()))  => '((1 5) () (1 7.8) ()))
(test (min&max-lists '((1) ()))  => '((1 1) ()))
(test (min&max-lists '(("fFAGG" 9 'DEF 2.5) (1.9 "Fd" 4 4.1 4/3) ( 0 "Fd" 0 -1 2/3)))  => '((2.5 9) (4/3 4.1) (-1 2/3)))
(test (min&max-lists '(("fFAGG" 'DEF 3.11) (0)))  => '((3.11 3.11) (0 0)))
(test (min&max-lists '()) => '())
(test (min&max-lists '((any "Benny" 10 OP 8) (any "Benny" OP (2 3)))) => '((8 10) ()))
(test (min&max-lists '((2 5 1 5 L) (4 5 6 7 3 2 1) ())) => '((1 5) (1 7) ()))
(test (min&max-lists '(() () ())) => '(() () ()))
(test (min&max-lists '((2))) => '((2 2)))
(test (min&max-lists '((2) ("test" (1 2)))) => '((2 2) ()))
(test (min&max-lists '((2 2) ('q) (-1 -1 1 1))) => '((2 2) () (-1 1)))
(test (min&max-lists '(() ("test") ('a))) => '(() () ()))
(test (min&max-lists '((E S S A L) (a A Z E R T 1) ())) => '(() (1 1) ()))
(test (EmptyKS) => (EmptyKS))
(test (Push 'a "a" (Push 'A "A" (EmptyKS))) => (Push 'a "a" (Push 'A "A" (EmptyKS))) )
(test (Push 'aaa "AA" (Push 'bbb "BBB" (Push 'aaa "AAA" (EmptyKS)))) =>(Push 'aaa "AA" (Push 'bbb "BBB" (Push 'aaa "AAA" (EmptyKS)))))
(test (search-stack 'aaa (Push 'aaa "AA" (Push 'bbb "BBB" (Push 'aaa "AAA" (EmptyKS))))) => "AA")
(test (search-stack 'f (Push 'a "A" (Push 'b "B" (Push 'c "c" (Push 'd "d" (Push 'e "e" (Push 'f "f" (EmptyKS)))))))) => "f")
(test (search-stack 'g (Push 'a "A" (Push 'b "B" (Push 'c "c" (Push 'd "d" (Push 'e "e" (Push 'f "f" (EmptyKS)))))))) => #f)
(test (search-stack 'E (Push 'EEE "EEE" (Push 'V "V" (Push 'Q "QQ" (EmptyKS))))) => #f)
(test (search-stack 'E (EmptyKS)) => #f)
(test (search-stack 'b (Push 'a "AAA" (Push 'b "Bisli" (Push 'a "Bamba"(EmptyKS))))) => "Bisli")
(test (search-stack 'cb (Push 'cb "LIAV" (Push 'b "B" (Push 'a "A" (EmptyKS))))) => "LIAV")

(test (pop-stack (Push 'aaa "AA" (Push 'bbb "BBB" (Push 'aaa "AAA" (EmptyKS))))) => (Push 'bbb "BBB" (Push 'aaa "AAA" (EmptyKS))))
(test (pop-stack (Push 'A "A" (EmptyKS))) => (EmptyKS))
(test (pop-stack (EmptyKS)) => #f)


