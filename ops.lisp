;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*-
;;; Module: ops.lisp
;;; different worlds and operators for the GPS planner.

(in-package :user)

(defstruct op "An GPS operator"
  (action nil) 
  (preconds nil) 
  (add-list nil) 
  (del-list nil))

(defun executing-p (x)
  "Is x of the form: (execute ...) ?"
  (starts-with x 'execute))

(defun convert-op (op)
  "Make op conform to the (EXECUTING op) convention."
  (unless (some #'executing-p (op-add-list op))
    (push (list 'execute (op-action op)) (op-add-list op)))
  op)

(defun op (action &key preconds add-list del-list)
  "Make a new operator that obeys the (EXECUTING op) convention."
  (convert-op
    (make-op :action action :preconds preconds
             :add-list add-list :del-list del-list)))

;;; ================= Sussman's Anomaly ====================

(defparameter *blocks-world* '(a-on-t b-on-t c-on-a clear-c clear-b))

(defparameter *blocks-ops*
  (list
    (make-op :action 'put-b-from-t-on-c
      :preconds '(b-on-t clear-c clear-b c-on-t)
      :add-list '(b-on-c)
      :del-list '(b-on-t clear-c))
    (make-op :action 'put-a-from-t-on-b
      :preconds '(a-on-t clear-b clear-a b-on-c)
      :add-list '(a-on-b)
      :del-list '(clear-b a-on-t))
    (make-op :action 'put-c-from-a-on-t
      :preconds '(c-on-a clear-c a-on-t)
      :add-list '(c-on-t clear-a)
      :del-list '(c-on-a))))

(defparameter *registration-world* '())
(defparameter *registration-ops*
  (list
    (make-op :action 'take-math-1050
      :add-list '(math-1050))
    (make-op :action 'take-math-1060
      :add-list '(math-1060))
    (make-op :action 'take-cs-1400
      :preconds '(math-1050)
      :add-list '(cs-1400))
    (make-op :action 'take-cs-1410
      :preconds '(cs-1400)
      :add-list '(cs-1410))
    (make-op :action 'take-cs-1440
      :preconds '(cs-1400)
      :add-list '(cs-1440))
    (make-op :action 'take-cs-2410
      :preconds '(cs-1410)
      :add-list '(cs-2410))
    (make-op :action 'take-cs-2420
      :preconds '(cs-1410)
      :add-list '(cs-2420))
    (make-op :action 'take-cs-2610
      :preconds '(cs-1410)
      :add-list '(cs-2610))
    (make-op :action 'take-math-1210
      :preconds '(math-1050 math-1060)
      :add-list '(math-1210))
    (make-op :action 'take-math-2270
      :preconds '(math-1210)
      :add-list '(math-2270))
    (make-op :action 'take-stat-3000
      :preconds '(math-1210)
      :add-list '(stat-3000))
    (make-op :action 'take-math-3310
      :preconds '(math-1210)
      :add-list '(math-3310))
    (make-op :action 'register-for-professional-program
      :preconds '(cs-1400 cs-1410 cs-1440 cs-2410 cs-2420 cs-2610 math-1210 math-3310)
      :add-list '(admitted))
    (make-op :action 'take-cs-2810
      :preconds '(admitted cs-1410)
      :add-list '(cs-2810))
    (make-op :action 'take-cs-3100
      :preconds '(admitted cs-2420)
      :add-list '(cs-3100))
    (make-op :action 'take-cs-3450
      :preconds '(admitted cs-2420 cs-2610)
      :add-list '(cs-3450))
    (make-op :action 'take-cs-4700
      :preconds '(admitted cs-2420)
      :add-list '(cs-4700))
    (make-op :action 'take-cs-5300
      :preconds '(admitted cs-2810)
      :add-list '(cs-5300))
    (make-op :action 'take-cs-5000
      :preconds '(admitted cs-2420)
      :add-list '(cs-5000))
    (make-op :action 'take-cs-5050
      :preconds '(admitted cs-2420)
      :add-list '(cs-5050))
    (make-op :action 'take-cs-5000
      :preconds '(admitted cs-2420)
      :add-list '(cs-5000))
    (make-op :action 'take-engl-3085
      :preconds '(admitted)
      :add-list '(engl-3085))
    (make-op :action 'take-cs-3200
      :preconds '(admitted cs-2410)
      :add-list '(cs-3200))
    (make-op :action 'take-cs-3430
      :preconds '(admitted cs-1410 math-1210)
      :add-list '(cs-3430))
    (make-op :action 'take-cs-3460
      :preconds '(admitted cs-1440 cs-2420)
      :add-list '(cs-3460))
    (make-op :action 'take-cs-4250
      :preconds '(admitted)
      :add-list '(cs-4250))
    (make-op :action 'take-cs-4320
      :preconds '(admitted cs-2420)
      :add-list '(cs-4320))
    (make-op :action 'take-cs-4460
      :preconds '(admitted cs-2420 cs-3100)
      :add-list '(cs-4460))
    (make-op :action 'take-cs-4610
      :preconds '(admitted cs-2610)
      :add-list '(cs-4610))
    (make-op :action 'take-cs-4950
      :preconds '(admitted cs-2420)
      :add-list '(cs-4950))
    (make-op :action 'take-cs-5030
      :preconds '(admitted cs-2420)
      :add-list '(cs-5030))
    (make-op :action 'take-cs-5040
      :preconds '(admitted cs-2420)
      :add-list '(cs-5040))
    (make-op :action 'take-cs-5060
      :preconds '(admitted cs-2420)
      :add-list '(cs-5060))
    (make-op :action 'take-cs-5080
      :preconds '(admitted cs-2420)
      :add-list '(cs-5080))
    (make-op :action 'take-cs-5110
      :preconds '(admitted cs-2420)
      :add-list '(cs-5110))
    (make-op :action 'take-cs-5140
      :preconds '(admitted)
      :add-list '(cs-5140))
    (make-op :action 'take-cs-5250
      :preconds '(admitted cs-2420)
      :add-list '(cs-5250))
    (make-op :action 'take-cs-5260
      :preconds '(admitted cs-5250)
      :add-list '(cs-5260))
    (make-op :action 'take-cs-5300
      :preconds '(admitted cs-2810)
      :add-list '(cs-5300))
    (make-op :action 'take-cs-5400
      :preconds '(admitted cs-2420 math-2270)
      :add-list '(cs-5400))
    (make-op :action 'take-cs-5410
      :preconds '(admitted cs-2420 cs-3100)
      :add-list '(cs-5410))
    (make-op :action 'take-cs-5470
      :preconds '(admitted cs-2420 math-2270)
      :add-list '(cs-5470))
    (make-op :action 'take-cs-5510
      :preconds '(admitted cs-2420)
      :add-list '(cs-5510))
    (make-op :action 'take-cs-5600
      :preconds '(admitted cs-2420)
      :add-list '(cs-5600))
    (make-op :action 'take-cs-5640
      :preconds '(admitted cs-2420)
      :add-list '(cs-5640))
    (make-op :action 'take-cs-5665
      :preconds '(admitted cs-2420)
      :add-list '(cs-5665))
    (make-op :action 'take-cs-5680
      :preconds '(admitted cs-2420 math-2270 stat-3000)
      :add-list '(cs-5680))
    (make-op :action 'take-cs-5700
      :preconds '(admitted cs-3450)
      :add-list '(cs-5700))
    (make-op :action 'take-cs-5750
      :preconds '(admitted stat-3000)
      :add-list '(cs-5750))
    (make-op :action 'take-cs-5800
      :preconds '(admitted cs-2420)
      :add-list '(cs-5800))
    (make-op :action 'take-cs-5820
      :preconds '(admitted cs-2420)
      :add-list '(cs-5820))
    (make-op :action 'take-cs-5830
      :preconds '(admitted cs-2420)
      :add-list '(cs-5830))
    (make-op :action 'take-cs-5840
      :preconds '(admitted cs-2420 math-2270 cs-5665)
      :add-list '(cs-5840))
    (make-op :action 'take-cs-5850
      :preconds '(admitted cs-2420)
      :add-list '(cs-5850))
    (make-op :action 'take-cs-5890
      :preconds '(admitted cs-2420)
      :add-list '(cs-5890))
    (make-op :action 'take-cs-5950
      :preconds '(admitted cs-2420)
      :add-list '(cs-5950))
))
  
(mapc #'convert-op *blocks-ops*)
(mapc #'convert-op *registration-ops*)
(provide :ops)
