;; compiler const
;; :D refere to "#" 
(defun compiler-const (exp)
;; MOVE  #<cste> R0 	
  `((MOVE (:D ,exp) :R0))
  )
  
;;compiler global variable
(defun compiler-varg (exp)
;;MOVE  *@<varg> R0
  `((MOVE (:* :@ ,exp) :R0))
  )

;;compile constant  or global variable
;;cell is valid in current env
(defun compiler-litteral (exp env);;association list env
  (let ((cell (assoc exp env)))
    (cond
    ;;symbol not null
     ((not (null cell))
      (if (eql (cadr cell) 'loc) 
	  `((MOVE ,(cdr cell) :R0))
	  ;; if constant atom
	(if (and (atom expr) (constantp expr))
	;;call compile constant
	    (compiler-const (cdr cell)))
	)
      )
      ;; no constant atom, so variable
     ((and (symbolp exp) (not (null exp))) 
     ;;compile global variable
     (compiler-varg exp))
     ;; garbage collector
     (t (compiler-const exp))
     )
    )
  )

;; compiler comparator 
;;

(defun compiler-comparator (exp env fenv fctname)
  (let ((op (car exp));;retrive the operator
	(end (gensym "endTest")));;gen dynamic symbol to identify all comparator expressions
    (append (compiler (cadr exp) env fenv fctname);;compile first item
	    '((PUSH :R0)) 
	    (compiler (caddr exp) env fenv fctname);;compile the second item
	    '((PUSH :R0))
	    '((POP :R0))
	    '((POP :R1))
	    '((CMP :R1 :R0));;compare values
	    '((MOVE (:D T) :R0)) ;;true = 1
	    (case op ;;complile the operator
	      ('= `((JEQ (@ ,end))))
	      ('< `((JL (@ ,end))))
	      ('> `((JG (@ ,end))))
	      ('<= `((JLE (@ ,end))))
	      ('>= `((JGE (@ ,end))))
	      )
	    '((MOVE (:D NIL) :R0));;false = 0
	    `((@ ,end)));;end label
    )
  )

  ;; compiler recursive body

(defun compiler-body (exp env fenv fctname)
    ;;stop condition if exp is empty
  (if (null exp) 
      ();;return empty
      ;;else if recursive call
    (append (compiler (car exp) env fenv fctname)
    (compiler-body (cdr exp) env fenv fctname))
    )
  )


  ;; compiler if condition.
  ;; ( if ( op X1 X2 ) A B )
(defun compiler-if (exp env fenv fctname)
  (let ((endIf (gensym "endIf"));;need to manage label end if
       (else (gensym "else")));;need to manage label else if
    (append (compiler (car exp) env fenv fctname)  
	    '((CMP :R0 (:D nil)));;if R0 = 0, so test false 
	    `((JEQ (@ ,else)));; jump to else block condition 
	    (compiler (cadr exp) env fenv fctname)
	    `((JMP (@ ,endIf))) 
	    `((@ ,else))  
	    (compiler (caddr exp) env fenv fctname)
	    `((@ ,endIf))) ;;end label
    )
  )

  ;; compiler defun fuctions

(defun compiler-function (exp env fenv fctname)
  (let ((n (assoc fctname fenv)));;n niveau d'emboitement
    (append '((FENTRY));;do nothing
	    `((@ ,(car exp)))
	    ;;compile body progn
	    (compiler-body (cddr exp) 
	                       ;; at first set niveau-emboitement (n) to 0
			       (make-env (cadr exp) env 1 (if n (+ 1 (cadr n)) 0)) 
			       ;;compile env inside function
			       (fun-env (list exp) fenv (if n (+ 1 (cadr n)) 0)) 
			       (car exp))
	    '((RTN));;return
	    '((FEND)));;do nothing
    )
  )

;;make environnement
(defun make-env (exp env begin n) ;;n niveau d'emboitement
;;if atom stay in the same environnement
  (if (atom exp) 
      env
      ;;else compile in nested environement
    (make-env (cdr exp) (cons (cons (car exp) `(loc ,(- 0 begin) ,n)) env) (+ 1 begin) n)
    )
  )
  


(defun fun-env (exp fenv n) ;;n niveau d'emboitement
  (if (atom exp) 
      fenv
    (fun-env (cdr exp) (cons `(,(caar exp) ,n) fenv) n)
    )
  )

;;compile call function
(defun compiler-call-function (exp env fenv fctname)
  (let ((n (length (cdr exp))) ;;count number of arguments
	(cell (assoc (car exp) fenv))) ;;local function
    (append (compiler-param (cdr exp) env fenv fctname)
	    `((PUSH (:D ,n))) ;;push nb arguments
	    `((MOVE :FP :R1)) ;;old FP
	    `((MOVE :SP :FP)) ;;new FP = SP
	    `((MOVE :SP :R2)) ;;get old SP
	    `((SUB  (:D ,n) :R2));; R2 = FP - (n + 1) + i
	    `((SUB  (:D 1) :R2));; save old SP
	    `((PUSH :R2)) ;;save old SP
	    `((PUSH :R1)) ;;end
	    (if cell  `((PUSH (:D ,(cadr cell)))) 
	     `((PUSH (:D ,0))));;restore
	    `((JSR (@ ,(car exp)))))
    )
  )

;;compiler parameters
(defun compiler-param (exp env fenv fctname)
  (if (atom exp)
      ();;return null
    (append (compiler (car exp) env fenv fctname)
	    `((PUSH :R0))
	    (compiler-param (cdr exp) env fenv fctname))
    )
  )
  
  ;;while compile
(defun compiler-while (exp env fenv fctname)
  (let ((end (gensym "endwhile"))
	(loops (gensym "while"))) 
    (if (eql (cadr exp) 'do) 
	(append `((@ ,loops))
		(compiler (car exp) env fenv fctname) 
		`((CMP :R0 (:D nil)))
		`((JEQ (@ ,end)))
		(compiler (caddr exp) env fenv fctname) 
		`((JMP (@ ,loops)))
		`((@, end)))
      (error "error: ~s" exp)
      )
    )
  )
  
   ;;make environnement
  (defun make-local-env (exp env begin n) ;;n niveau d'emboitement 
  (if (atom exp) ;if atom stay in the same environnement
      env;;else compile in nested environement
    (make-local-env (cdr exp) (cons (cons (caar exp) `(loc ,begin ,n)) env) (+ 1 begin) n)
    )
  )
  ;;compile let
  (defun compiler-let-v (exp env fenv fctname)
  (let ((n (assoc fctname fenv)))
    (append (compiler-push (car exp) env fenv fctname)
	    (compiler (cadr exp) (make-local-env (car exp) env 1 (cadr n)) fenv fctname)
	    (compiler-pop (car exp) env fenv fctname))
    )
  )

(defun compiler-push (exp env fenv fctname)
  (if (null exp) 
      ()
    (append (compiler (cadar exp) env fenv fctname)
	    '((PUSH :R0))
	    (compiler-push (cdr exp) env fenv fctname ))
    )
  )

(defun compiler-pop (exp env fenv fctname)
  (if (null exp) 
      ()
    (append '((POP :R1)) (compiler-pop (cdr exp) env fenv fctname))
    )
  )

  ;; compiler operation; ( + - * / )
  ;;(op X1 X2)

(defun compiler-op (exp env fenv fctname)
  (let ((op (car exp))
	(exp2 (cdr exp)))
    (if (null (cddr exp2))
    ;;simple expression like (+ X1 X2)
	(append (compiler (car exp2) env fenv fctname);; generate asm for X1
		'((PUSH :R0))
		(compiler (cadr exp2) env fenv fctname);; ;; generate asm for X2
		'((PUSH :R0))
		'((POP :R1))
		'((POP :R0))
		(case op
		  ('+ '((ADD :R1 :R0))) ;x+y
		  ('- '((SUB :R1 :R0))) ;x-y
		  ('* '((MULT :R1 :R0))) ;x*y
		  ('/ '((DIV :R1 :R0))))) ;x/y  
		   ;;composite expression like (+ (+ X1 X2) (+ Y1 Y2) )
      (append (compiler  `(,op ,(list op (car exp2) (cadr exp2)) ,@(cddr exp2)) env fenv fctname))
      )
    )
  )


;; general compiler 
;;take expression, environnement, local function, and fonction name
(defun compiler (exp &optional (env ()) (fenv ())  (fctname ()) )
  (let ((exp2 (if (atom exp) () (cdr exp))))
    (cond ;;case analysis
    ;; literal constante and variable
     ((or (atom exp) (constantp exp)) (compiler-litteral exp env))
     ;;if condition
     ((eql (car exp) 'if) (compiler-if exp2 env fenv fctname))
     ;; global function
     ((eql (car exp) 'defun) (compiler-function exp2 env fenv fctname))
     ;; operator
     ((member (car exp) '(+ - * /)) (compiler-op exp env fenv fctname))
     ;;comparator
     ((member (car exp) '(< > = <= >= )) (compiler-comparator exp env fenv fctname))
     ;;while
     ((eql (car exp) 'while) (compiler-while exp env fenv fctname))
     ;;let
     ((eql (car exp) 'let) (compiler-let-v exp env fenv fctname))
     ;; call function
     (`(function ,(car exp)) (compiler-call-function exp env fenv fctname))
    )
    )
  )
