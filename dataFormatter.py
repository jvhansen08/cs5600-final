import json

preReqs = {}
with open("allData.json", 'r') as f:
    preReqs = json.load(f)

lispProgram = """(in-package :user)

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
(defparameter *registration-world* '())
(defparameter *registration-ops*
  (list
"""

for k, v in preReqs.items():
    preconds = " ".join(v)
    lispCommand = f"\n\t\t(make-op :action 'take-{k}"
    if len(preconds) > 0:
        lispCommand += f"\n\t\t\t:preconds '({preconds})"
    lispCommand += f"\n\t\t\t:add-list '({k}))"
    lispProgram += lispCommand

lispProgram += """))
(mapc #'convert-op *registration-ops*)
(provide :ops)
"""

with open("ops2.lisp", "w") as f:
    f.write(lispProgram)
