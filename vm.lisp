
;;load run compile any code
  (defun compile-load-run (vm code)
  (load-vm vm (compiler code));;compile and load asm
  (run-vm vm);;run vm
  )

;;reset vm
;;machine name and size; set 1500 by default
(defun reset-memory (&optional (name 'machin) (sze 1500))
    (set-size name sze);;set size
    (set-property name :memarray (make-array sze));;make array vm
    (set-program-counter name (- sze 1));;;;begin position 1
    (set-load-counter name (- sze 1));;begin position 1
    (set-property name :label (make-hash-table :size sze));;label
    (set-property name :label-n-r (make-hash-table :size sze));;label not resolve
    (set-label-n-r name 'n 0);;begin position 0
    
  )

(defun make-vm (&optional (name 'machin) (msizet 1500))
  (set-property name :name name)
  (set-property name :R0 0);;register
  (set-property name :R1 0);;register
  (set-property name :R2 0);;register
  (set-property name :R3 0);;register
  (set-property name :BP 1);;base pointer
  (set-property name :SP 1);;stack pointer
  (set-property name :VARP 1);;pointer global var
  (set-property name :FP 0);;frame poiter
  (set-property name :DPP 0);;flag
  (set-property name :DE 0)
  (set-property name :DPG 0)
  (reset-memory name msizet);;reset
  )

  ;; load asm in vm
;;vm name, asm after compile
(defun load-vm (vm asm)
  (let ((exp asm)
	(inst (car asm))
	(label (make-hash-table :size (get-size vm)));;load label adress
	(labelnr (make-hash-table :size (get-size vm))));;load not solve adresses
    (set-hash labelnr 'n 0);;start 0
    (loop while exp
	  do
	  (case (car inst);;case analyse
	    ('@ (set-adress vm exp inst label labelnr));; labels
	    ('VARG (set-adress-varg vm exp inst));;global v
	    ('JSR (set-adress-jumps vm exp inst));;jumps
	    ('FEntry (set-adress-funct vm exp inst));;new function
	    (otherwise (set-adress-other vm exp inst label labelnr));;expl simple instruction
	    )
	  do (setf exp (cdr exp));;next
	  do (setf inst (car exp));;next
	  )
    )
  )
  
;; run vm
(defun run-vm (&optional (name 'machin) (print-res ()))
  (set-memory-load-counter name '(HALT))
  (let ((nbrf 0))
  (loop while (vm-running name)
	do
	(if (instruction-function name) 
	    (exc-function name nbrf)
	  (run-instruction name (get-memory-program-counter name) print-res)
	  )
	)
  )
  (if (stack-error name) 
      (error "stack")
    (get-register name :R0))
  )

(defun run-instruction (vm exp &optional (print-res ()))
  (let ((inst (car exp))
	(src1 (cadr exp))
	(src2 (caddr exp)))
    (if (null exp) ;;do nothing if null
	(nop-instruction vm)
      (case inst ;;case analysis
	('move (move-instruction vm src1 src2))
	('add (add-instruction vm src1 src2))
	('sub (sub-instruction vm src1 src2))
	('mult (mul-instructiont vm src1 src2))
	('div (div-instruction vm src1 src2))
	('JMP  (jmp-instruction vm src1))
	('CMP  (vm-cmp vm src1 src2))
	('JEQ (jeq-instruction vm src1))
	('JL (jl-instruction vm src1))
	('JLE (jle-instruction vm src1))
	('JG (jg-instructionvm src1))
	('JGE (jge-instruction vm src1))
	('JNE (jne-instruction  vm src1))
	('JSR (jsr-instruction vm src1))
	('PUSH (push-instruction vm src1))
	('POP (pop-instruction vm src1))
	('INCR (increment-instruction vm src1))
	('DECR (decrement-instruction vm src1))
	('RTN (rtn-instruction vm))
	('FENTRY (nop-instruction vm))
	('FEND (nop-instruction vm))
	(otherwise (print-error vm exp))
	)
   )
    (if (not (null print-res)) (format t "~S~%" (get-memory-program-counter vm)))
    (decrement-program-counter vm)
    )
  )

;; stack instructions
;;SP Stack Pointer
(defun push-instruction (vm src)
  (increment-stack-pointer vm) ;;incremente SP
  (move-instruction vm src '(:* :SP)) ;;put <src> on new SP adress
  )

(defun pop-instruction (vm dest) ;;SP Stack Pointer
  (move-instruction vm '(:* :SP) dest) ;;put SP content on <dest>
  (decrement-stack-pointer vm) ;;decremente SP
  )

;;arithmetic instructions
;;ADD <src> <dest>
(defun add-instruction (vm src dest)
  (operation vm '+ src dest)
  )

;;SUB <src> <dest>
(defun sub-instruction (vm src dest)
  (operation vm '- src dest)
  )

;; MULT <src> <dest>
(defun mul-instructiont (vm src dest)
  (operation vm '* src dest)
  )

;;DIV <src> <dest>
(defun div-instruction (vm src dest)
  (operation vm '/ src dest)
  )


;; increment and decrement

(defun increment-instruction (vm dest)
  (add-instruction vm '(:D 1) dest) ;;ADD #1 <dest>
  )

(defun decrement-instruction (vm dest)
  (sub-instruction vm '(:D 1) dest) ;; SUB #1 <dest>
  )

;; jump

;;JMP <dest> :
(defun jmp-instruction (vm dest)
  (if (numberp dest)  
      (set-program-counter vm dest) ;;affect program counter to <dest>. MOVE <dest> PC
    (move-instruction vm dest :PC)
    )
  )

;;JSR <label>
(defun jsr-instruction (vm label)
  (push-instruction vm :PC) ;;push current adress on stack
  (jmp-instruction vm label) ;;jump
  ) 


;;CMP <src1> <src2>
(defun vm-cmp (vm src1 src2)
  (let ((result1 (get-src vm src1))
	(result2 (get-src vm src2)))
    (if (and (numberp result1) (numberp result2)) ;;compare
	(cond
	 ((eql result1 result2) (set-flag vm 0 1 0)) ;;010 (if <src1> = <src2>),
	 ((< result1 result2) (set-flag vm 1 0 0)) ;;100 (if <src1> < <src2>),
	 ((> result1 result2) (set-flag vm 0 0 1)) ;;001 (if <src1> > <src2>),
	 )
      (if (eql result1 result2) 
	      (set-flag vm 0 1 0)
      	(set-flag vm 0 0 0)
	)
      )
    )
  )

;; JUMP JL, JEQ, JG, JLE, JGE, JNE <dest>

;;JL <dest>
(defun jl-instruction (vm dest)
  (vm-jcond vm dest 'j-least) ;;least = 100 
  )
;; JEQ <dest>
(defun jeq-instruction (vm dest)
  (vm-jcond vm dest 'j-egal) ;;equal = 010 
  )
;; JG <dest>
(defun jg-instruction(vm dest)
  (vm-jcond vm dest 'j-greater) ;;greater = 001
  )
;; JLE <dest>
(defun jle-instruction (vm dest)
  (vm-jcond vm dest 'jle-least-or-egal) ;;least or egal = 110
  )

;; JGE  <dest>
(defun jge-instruction (vm dest)
  (vm-jcond vm dest 'jge-greater-or-egal) ;;JGE = 011
  )

;;JNE  <dest>
(defun jne-instruction (vm dest)
  (vm-jcond vm dest 'jne-not-egal) ;;JNE  = 101
  )


;; rtn instruction

(defun rtn-instruction (vm)
  (move-instruction vm '( 1 :FP) :SP)
  (move-instruction vm '( 4 :FP) :PC)
  (move-instruction vm '( 2 :FP)  :FP)
  )

;;NOP 
(defun nop-instruction (vm)
;;Do nothing
  )

;;check operation type
;;instruction
(defun instruction-ope (vm inst)
  (eql (car (get-memory-program-counter vm)) inst)
  )
;;function begin
(defun instruction-function (vm)
  (instruction-ope vm 'FENTRY)
  )
;;function end
(defun instruction-function-end (vm)
  (instruction-ope vm 'FEND)
  )
;;excute function
;;nr FENTRY
(defun exc-function (vm nbrf)
  (setf nbrf (+ nbrf 1));;nested FENTRY
  (loop while (< 0 nbrf)
	do 
	(decrement-program-counter vm)
	(cond
	 ((instruction-function vm) (setf nbrf (+ nbrf 1)))
	 ((instruction-function-end vm) (setf nbrf (- nbrf 1)))
	 )
	)
  )
;;for all operation + -
(defun operation (vm op src dest)
  (let ((adr (get-dest vm dest))
	(res (apply op (list (get-src vm (get-dest vm dest)) (get-src vm src)))))
    (set-register vm adr res)
    )
  )

(defun vm-jcond (vm dest cond)
  (if (apply cond (list vm)) 
      (jmp-instruction vm dest))
  )

  ;; JUMP

(defun is-jmp (instruction)
  (member (car instruction) '(JMP JEQ JL JG JLE JGE JNE))
  )
;;local label and not solved
(defun set-adress (vm exp inst label labelnr)
  (if (get-hash label (cadadr exp))
      (error "much lab : ~S" (cadr inst))
    (progn
      (set-hash label (cadr inst) (+ (get-load-counter vm) 1));;load label
      (more-label vm (get-hash labelnr (cadr inst)) (+ (get-load-counter vm) 1))
      (if (get-hash labelnr (cadr inst))
	  (progn
	    (set-hash labelnr (cadr inst) ())
	    (increment-hash labelnr)
	    )
	)
      )
    )
  )
;;var g progrogram
(defun set-adress-varg (vm exp inst)
  (if (null (get-label vm (cadr inst)))
      (progn
	(set-label vm (cadr inst) (get-property vm :VARP))
	(if (< (get-property vm :VARP) (- (get-property vm :BP) 1)) 
	    (increment-prop vm :VARP)
	  )
	)
    )
  )
;;foe jump
(defun set-adress-jumps (vm exp inst)
  (if (get-label vm (cadadr inst))
      (set-memory-load-counter vm (list (car inst) (get-label vm (cadadr inst))))
    (progn                                    
      (if (null (get-label-n-r vm (cadadr inst)))
	  (increment-label-n-r vm)
	)
      (set-label-n-r vm (cadadr inst) (list* (get-load-counter vm) (get-label-n-r vm (cadadr inst))))
      (set-memory-load-counter vm inst)
      )
    )
  (decrement-load-counter vm)
  )
;;for fonction
(defun set-adress-funct (vm exp inst)
  (if (get-label vm (cadadr exp))
      (error "already exit : ~S" (cadadr exp))
    (progn
      (set-memory-load-counter vm inst)
      (set-label vm (cadadr exp) (get-load-counter vm))
      (more-label vm (get-label-n-r vm (cadadr exp)) (get-load-counter vm))
      (if (get-label-n-r vm (cadadr exp))
	  (progn
	    (set-label-n-r (cadadr exp) ())
	    (increment-label-n-r vm)
	    )
	)
      (decrement-load-counter vm)
      )
    )
  )
;;label
(defun set-adress-other (vm exp inst label labelnr)
  (if (is-jmp inst)
      (progn
	(if (get-hash label (cadadr inst))
	    (set-memory-load-counter vm (list (car inst) (get-hash label (cadadr inst))))
	  (progn
	    (if (null (get-hash labelnr (cadadr inst)))
		(increment-hash labelnr)
	      )
	    (set-hash labelnr (cadadr inst) (list* (get-load-counter vm) (get-hash labelnr (cadadr inst))))
	    (set-memory-load-counter vm inst)
	    )
	  )
	)
    (set-memory-load-counter vm inst)
    )
  (decrement-load-counter vm)
  )

  ;; three flag

(defun set-flag (vm dpp de dpg)
  (set-property vm :DPP dpp)
  (set-property vm :DE de)
  (set-property vm :DPG dpg)
  )

(defun j-egal (vm) ;;=
  (eql (get-property vm :DE) 1)
  )

(defun jne-not-egal (vm) ;!=
  (not (j-egal vm))
  )

(defun j-least (vm)
  (eql (get-property vm :DPP) 1) ;<
  )

(defun jge-greater-or-egal (vm) ;>=
  (not (j-least vm))
  )

(defun j-greater (vm)
  (eql (get-property vm :DPG) 1) ;>
  )

(defun jle-least-or-egal (vm) ;<=
  (not (j-greater vm)) 
  )

;;vm structure
(defun is-prop (exp) 
  (member exp (list :R0 :R1 :R2 :R3 :SP :VARP :FP :DPP :DE :DPG :PC :LC))
  )
;;verify constant
(defun is-constant (exp)
  (eql (car exp) :D)
  )

(defun is-index (exp)
  (numberp (car exp))
  )
;;get index
(defun get-index (vm exp)
  (get-memory vm (+ (car exp) (get-src vm (cadr exp))))
  )
;;verify label
(defun is-label (exp) 
  (eql (car exp) :@)
  )
;;indirect adress
(defun is-indir-adress (exp) 
  (eql (car exp) :*)
  )

(defun get-indir-src (vm exp)
  (get-memory vm (if (null (cddr exp)) 
		  (get-src vm (cadr exp)) 
		(get-src vm (cdr exp)))
	   )
  )

(defun get-indir-dest (vm exp)
  (if (null (cddr exp))
      (get-src vm (cadr exp))
    (if (is-index (cdr exp))
	(+ (cadr exp) (get-src vm (caddr exp)))
      (if (is-label (cdr exp)) 
	  (get-src vm (cdr exp))
	)
      )
    )
  )

(defun is-loc (exp)
  (eql (car exp) 'LOC)
  )
;;get new frame pointer
(defun get-new-FP (vm exp)
  (let ((newFP (get-frame-pointer vm)))
    (loop while (> (get-memory vm (+ 3 newFP)) (caddr exp))
	  do (setf newFP (get-memory vm (+ 2 newFP)))
	  )
    newFP
    )
  )
;;find src adress
(defun get-loc-src (vm exp)
  (if (eql (caddr exp) ()) (setf (caddr exp) 0))
  (let ((newFP (get-new-FP vm exp)))
    (if (< (cadr exp) 0)
	(get-memory vm (- newFP (get-memory vm newFP) 1 (cadr exp)))
      (get-memory vm ( + 4 newFP (cadr exp)))
      )
    )
  )
;;find dest adress
(defun get-loc-dest (vm exp)
  (let ((newFP (get-new-FP vm exp)))
    (if (< (cadr exp) 0)
	(- newFP (get-memory vm newFP) 1 (cadr exp))  
      (+  4 newFP  (cadr exp))
      )
    )
  )


;; get content src dest
 
(defun get-src (vm exp)
  (if (atom exp)
      (cond
       ((null exp) 0)
       ((numberp exp) (get-memory vm exp))
       ((is-prop exp) (get-property vm exp))
       )
    (if (consp exp)
	(cond
	 ((is-constant exp) (cadr exp))
	 ((is-index exp) (get-index vm exp))
	 ((is-label exp) (get-label vm (cadr exp))) 
	 ((is-indir-adress exp) (get-indir-src vm exp))
	 ((is-loc exp) (get-loc-src vm exp))
	 )
      )
    )
  )

(defun get-dest (vm exp)
  (if (atom exp)
      (cond
       ((null exp) 0)
       ((numberp exp) exp)
       ((is-prop exp) exp)
       )
    (if (consp exp)
	(cond
	 ((is-index exp) exp)
	 ((is-label exp) (get-label vm (cadr exp)))
	 ((is-constant exp) (cadr exp))
	 ((is-indir-adress exp) (get-indir-dest vm exp))
	 ((is-loc exp) (get-loc-dest vm exp))
	 )
      )
    )
  )
  ;; VM run and excute
;;yes
(defun instruction-eval (vm)
  (consp (get-memory-program-counter vm))
  )
;;not end
(defun not-halt (vm)
  (not (instruction-ope vm 'HALT))
  )
;;vm running
(defun vm-running (&optional (vm 'machin))
  (and (instruction-eval vm)  (not-halt vm))
  )

(defun stack-error (vm)
  (>= (get-stack-pointer vm) (get-program-counter vm))
  )

