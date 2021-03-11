(defun print-error (vm exp)
  (format t "Error ~D" exp)
  )

;; getters and setters vm property

(defun get-property (vm prop)
  (get vm prop);;return prop value
  )

(defun set-property (vm prop value )
  (setf (get vm prop) value ) ;;set value on prop
  )

;; increment and decrement vm memory
(defun increment-prop (vm prop)
 ;;prop + 1
  (set-property vm prop (+ (get-property vm prop) 1))
  )

(defun decrement-prop (vm prop)
  ;;prop - 1
  (set-property vm prop (- (get-property vm prop) 1))
  )
;;set value on cell key
(defun set-tab (tab key value )
  (setf (aref tab key) value ) ;; tab[key] = value
  )


;; memory manager getters and setters
;;get and set memory size
;;size example 10000 by default
(defun get-size (&optional (vm 'machin))
  (get-property vm :sze)
  )

(defun set-size (vm msizet)
  (set-property vm :sze msizet)
  )
;;memory array
(defun get-memory (vm adr)
  (aref (get vm :memarray) adr)
  )
;;example memarray[adr] = value
(defun set-memory (vm adr value )
  (set-tab (get vm :memarray) adr value )
  )


;;register manager getters and setters
;;for R0 R1 R2 ...

(defun get-register (vm reg)
  (get-property vm reg)
  )

(defun set-register (vm reg value )
  (set-property vm reg value )
  )


;;vm instructions
;;MOVE <src> <dest>

(defun move-instruction (vm src dest)
  (let ((adr (get-dest vm dest))
	(res (get-src vm src)));;result
    (if (numberp adr)
	(set-memory vm adr res)
      (set-property vm adr res)
      )
    )
  )



;; hash table getters and setters

(defun get-hash (tab key)
  (gethash key tab) ;;tab[key]
  )

(defun set-hash (tab key value )
  (setf (gethash key tab) value ) ;;tab[key] = value
  )

(defun increment-hash (tab)
  (set-hash tab 'n (+ (get-hash tab 'n) 1));;tab[n] += 1
  )
;;getters setters label
(defun get-label (vm key)
  (get-hash (get-property vm :label) key);;expl vm :Label
  )
;;expl vm :Label = 0
(defun set-label (vm key value )
  (set-hash (get-property vm :label) key value )
  )

(defun increment-label (vm)
  (increment-hash (get-property vm :label))
  )
;;getters setters label no resolve
(defun get-label-n-r (vm key)
  (get-hash (get-property vm :label-n-r) key)
  )

(defun set-label-n-r (vm key value )
  (set-hash (get-property vm :label-n-r) key value )
  )

(defun increment-label-n-r (vm)
  (increment-hash (get-property vm :label-n-r))
  )

(defun more-label (vm exp adr)
  (if (null exp) 
      () ;;stop
    (progn ;;load in mmemory
      (set-memory vm (car exp) (list (car (get-memory vm (car exp))) adr))
      (more-label vm (cdr exp) adr) ;;load next
      )
    )
  )
;; PC program counter getters setters increment decremente

(defun get-program-counter (&optional (vm 'machin))
  (get-property vm :PC)
  )

(defun set-program-counter (vm value )
  (set-property vm :PC value )
)

(defun increment-program-counter (vm)
  (increment-prop vm :PC)
  )

(defun decrement-program-counter (vm)
  (decrement-prop vm :PC)
  )
 ;;LC load counter getters setters increment decremente
(defun get-load-counter (&optional (vm 'machin))
  (get-property vm :LC)
  )

(defun set-load-counter (vm value )
  (set-property vm :LC value )
)

(defun increment-load-counter (vm)
  (increment-prop vm :LC)
  )

(defun decrement-load-counter (vm)
  (decrement-prop vm :LC)
  )


  ;;SP stack pointer getters setters increment decremente

(defun get-stack-pointer (&optional (vm 'machin))
  (get-property vm :SP)
  )

(defun set-stack-pointer (vm value )
  (set-property vm :SP value )
)

(defun increment-stack-pointer (vm)
  (increment-prop vm :SP)
  )

(defun decrement-stack-pointer (vm)
  (decrement-prop vm :SP)
  )

;;FP frame pointer
(defun get-frame-pointer (&optional (vm 'machin))
  (get-property vm :FP)
  )

(defun set-frame-pointer (vm value )
  (set-property vm :FP value )
)

;;memory instruction getters setters PC memarray

(defun get-memory-program-counter (&optional (vm 'machin))
  (get-memory vm (get-program-counter vm))
  )

(defun set-memory-program-counter (vm value )
  (set-memory vm (get-program-counter vm) value )
  )

;;PL memarray
(defun get-memory-load-counter (&optional (vm 'machin))
  (get-memory vm (get-load-counter vm))
  )

(defun set-memory-load-counter (vm value )
  (set-memory vm (get-load-counter vm) value )
  )