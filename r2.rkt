#lang racket

(module+ test
  (require rackunit rackunit/text-ui)
  )

(define/contract (calculator exp)
  (-> any/c number?)
  (match exp
    ((? number? x) x)
    (`(,op ,e1 ,e2)
     (let ((v1 (calculator e1))
           (v2 (calculator e2)))
       (match op
         ('+ (+ v1 v2))
         ('- (- v1 v2))
         ('* (* v1 v2))
         ('/ (/ v1 v2)))))
    ))
(module+ test
  (check-equal? (calculator '(+ 1 2)) 3)
  (check-equal? (calculator '(/ (* 3 7) (+ 1 2))) 7)
  (check-equal? (calculator '6) 6)
  )


;; R2
;; lcexp ::= id
;;         | (lambda (id) lcexp)
;;         | (lcexp lcexp)
;; id ::= symbol?
;; var ::= symbol?
;; procedure ::= '(lambda (var) lcexp)
;; binding ::= '(let ((x e1)) e2)
;; appliction ::= (e1 e2)
;; calculate ::= (op e1 e2)
;;
;; env
;; last in first out
;; env ::= '() | (extend-env (symbol? any/c env))
;; constructor
(struct extend-env (var value saved-env) #:transparent)

;; predicates
(define/contract (env? v)
  (-> any/c boolean?)
  (or (null? v)
      (extend-env? v)))
(module+ test
  (check-true (env? null))
  (check-true (env? (extend-env 'a 1 null)))
  )

(define/contract (empty-env? e)
  (-> env? boolean?)
  (null? e))
(module+ test
  (check-true (empty-env? null))
  (check-false (empty-env? (extend-env 'a 1 null)))
  )

;; extractor
(define/contract (apply-env var e)
  (-> symbol? env? any/c)
  (if (empty-env? e)
      (error 'apply-env "Undefined Variable %s" var)
      (if (eq? var (extend-env-var e))
          (extend-env-value e)
          (apply-env var (extend-env-saved-env e))))
  )
(module+ test
  (check-equal? (apply-env 'b (extend-env 'c 1
                                          (extend-env 'b 2 null)))
                2))

;; Closure
(struct Closure (function env) #:transparent)

;; evaluator
(define/contract (evaluator lcexp env)
  (-> any/c env? any/c)
  (match lcexp
    ((? number? x) x)
    ((? symbol? s) (apply-env s env))
    (`(lambda (,x) ,e)
     (Closure lcexp env))
    (`(let ((,x ,e1)) ,e2)
     (let ((v1 (evaluator e1 env)))
       (evaluator e2 (extend-env x v1 env))))
    (`(,e1 ,e2)
     ;; (let ((f (evaluator e1 env))
     ;;       (v2 (evaluator e2 env)))
     ;;   ((evaluator (Closure-function f) (Closure-env f)) v2))
     (let ((v2 (evaluator e2 env))
           (f (evaluator e1 env)))
       (match f
         ((Closure `(lambda (,x) ,e) saved-env)
          (evaluator e (extend-env x v2 saved-env)))))
     )
    (`(,op ,e1 ,e2)
     (let ((v1 (evaluator e1 env))
           (v2 (evaluator e2 env)))
       (match op
         ('+ (+ v1 v2))
         ('- (- v1 v2))
         ('* (* v1 v2))
         ('/ (/ v1 v2))
         )))))
(module+ test
  (check-equal? (evaluator 1 null) 1)
  (check-equal? (evaluator 'a (extend-env 'a 1 null)) 1)
  (check-equal? (evaluator `(let ((x 1)) (+ x 2)) null)
                3)
  )
;; UI of the interpreter
(define/contract (r2 lcexp)
  (-> any/c any/c)
  (evaluator lcexp null)
  )
(module+ test
  (check-equal? (r2 '(+ 1 2))
                3)
  (check-equal? (r2 '(* 2 3))
                6)
  (check-equal? (r2 '(* 2 (+ 3 4)))
                14)
  (check-equal? (r2 '(* (+ 1 2) (+ 3 4)))
                21)
  (check-equal? (r2 '((lambda (x) (* 2 x)) 3))
                6)
  (check-equal? (r2
                 '(let ([x 2])
                    (let ([f (lambda (y) (* x y))])
                      (f 3))))
                6)
  (check-equal? (r2
                 '(let ([x 2])
                    (let ([f (lambda (y) (* x y))])
                      (let ([x 4])
                        (f 3)))))
                6)
  )
