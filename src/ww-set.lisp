;;; Filename: ww-set.lisp

;;; User interface for resetting Wouldwork's search control parameters.

(in-package :ww)


;(defun function-lambda-expression@ (fn)
;  "Wrapper for function-lambda-expression which compiles it with (debug 3)."
;  (let ((incoming-debug (assoc 'debug (sb-cltl2:declaration-information 'optimize))))
;    (proclaim '(optimize (debug 3)))
;    (let ((lambda-expression (function-lambda-expression fn)))
;      (proclaim `(optimize ,incoming-debug))
;      lambda-expression)))


(defmacro ww-set (param val)
  "Allows resetting of user parameters during and after loading."
  (let ((param-name (symbol-name param)))
    `(case ',param
       ((*depth-cutoff* *tree-or-graph* *solution-type*
         *progress-reporting-interval* *randomize-search* *branch*)
        (progn (setq ,param ,(if (symbolp val) `',val val))
               (unless *ww-loading*
                 (let* ((str ,param-name)
                        (kwd (intern (subseq str 1 (1- (length str))) :keyword)))
                   (set-globals kwd ,(if (symbolp val) `',val val))))
               ,(if (symbolp val) `',val val)))
       (*debug*
        (progn (setq *debug* ,(if (symbolp val) `',val val))
               (if (or (> *debug* 0) *probe*)
                   (pushnew :ww-debug *features*)
                   (setf *features* (remove :ww-debug *features*)))
               (unless *ww-loading*
                 (set-globals :debug ,(if (symbolp val) `',val val)))
               ,(if (symbolp val) `',val val)))
       (*probe*
        (progn 
          (unless (listp ',val)
            (error "*probe* value must be an unquoted list"))
          (setq *probe* ',val)
          (setf *debug* 0)
          (setq *counter* 1)
          (unless *ww-loading*
            (set-globals :debug 0 :probe ',val))
          ',val))
       (*threads*
        '(block sbcl-test
           (progn (unless (member :sbcl *features*)
                    (format t "~%Note that multi-threading is not available unless running SBCL.~2%")
                    (return-from sbcl-test))
                  (format t "~%*threads* cannot be changed with ww-set.")
                  (format t "~%Instead, set its value in the file settings.lisp, and then exit and restart SBCL.~2%"))))
       ((*problem-name* *problem-type*)
        (progn (setq ,param ,(if (symbolp val) `',val val))
               (unless *ww-loading*
                 (format t "~%Please set the parameter ~A in the problem specification file, not in the REPL.~%" ',param))))
       (otherwise
        (format t "~%~A is not a valid parameter name in ww-set.~%" ',param)))))