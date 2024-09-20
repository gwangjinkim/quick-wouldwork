;;; Filename:  ww-parallel.lisp

;;; Collection of functions for managing parallelism (when requested by user).


(in-package :ww)


(defun process-threads ()
  "The main consumer of parallel thread processing."
  (setf lparallel:*kernel* (lparallel:make-kernel *threads*))
  (let ((channel (lparallel:make-channel))
        (problems (lparallel.queue:make-queue))  ;holds current problems to be solved
        (first (lparallel.queue:make-queue)))  ;signals first solution sufficient & found
    (setf *num-idle-threads* *threads*)  ;this main thread for control only, not one of the search *threads*
    (iter (for i from 1 to *threads*)  ;get all solving threads up & running
      (lparallel:submit-task channel #'search-parallel problems first))  ;start each thread running search-parallel
    (lparallel.queue:push-queue *open* problems)  ;setup the main problem for the thread pool
    (iter (sleep 0.1)  ;let threads work on problems
          (when (= *num-idle-threads* *threads*)  ;all threads are idle signifying end of search
            (leave)))
    (lparallel.queue:push-queue 'stop problems))  ;send stop message to all idle threads
  (lparallel:end-kernel :wait t))


(defun search-parallel (problems first)  ;follows outline in search-serial
  "Branch & Bound DFS parallel search in each thread."
  (iter
    (let ((open (lparallel.queue:pop-queue problems)))  ;open is local for this thread, blocks idling for a problem
      (when (eql open 'stop)
        (lparallel.queue:push-queue 'stop problems)  ;reinstate for other threads
        (return-from search-parallel))  ;close thread
      (increment-global *num-idle-threads* -1)  ;not idle any more
      #+:ww-debug (when (>= *debug* 1)
                    (let ((*package* (find-package :hs)))
                      (lprt *-* 'entering open)))
      (iter  ;process all nodes on this open
        (when (lparallel.queue:peek-queue first)  ;interrupt this thread when a thread signals first found
          #+:ww-debug (when (>= *debug* 1)
                        (lprt 'interrupted))
          (increment-global *num-idle-threads*)
          (leave))  ;don't continue processing, return to top to either pop next problem, idle, or stop
        (when (and (> (hs::length-hstack open) 1)   ;(not (linear open))
                   (> *num-idle-threads* 0))  ;causes multiple splitting
          (let ((subopen (split-off open)))   ;split open
            (when subopen
              #+:ww-debug (when (>= *debug* 1)
                            (let ((*package* (find-package :hs)))
                              (lprt 'splitting subopen open)))
              (lparallel.queue:push-queue subopen problems))))  ;continue processing this open after split-off
        (let ((current-node (hs::peek-hstack open))
              (succ-nodes (df-bnb1 open)))  ;work a little more on current problem
          (declare (ignorable current-node))
          (when (= *program-cycles* 0)  ;ie, expanding the start state
            (when (>= *branch* 0)  ;choose an initial branch to explore, drop others
              (format t "~&Exploring only branch ~D of ~D~%" *branch* (length succ-nodes))
              (setf succ-nodes (subseq succ-nodes *branch* (1+ *branch*))))
            (setf *num-init-successors* (length succ-nodes))
            (setf *rem-init-successors* (reverse succ-nodes)))
          #+:ww-debug (when (>= *debug* 1)
                        (lprt current-node))
          (when (equal succ-nodes '(first))  ;first solution sufficient & found
            #+:ww-debug (when (>= *debug* 1)
                          (lprt 'first-solution-found))
            (lparallel.queue:push-queue 'found first)  ;signal all threads to go to top (to pop or idle)
            (increment-global *num-idle-threads*)
            (leave))  ;go back to top
          (when succ-nodes
            (if (fboundp 'heuristic?)
              (setf succ-nodes
                (sort (copy-list succ-nodes) #'>
                      :key (lambda (node)
                             (problem-state.heuristic (node.state node)))))
              (when *randomize-search*
                (setf succ-nodes (alexandria:shuffle succ-nodes)))))
          #+:ww-debug (when (>= *debug* 1)
                        (lprt 'expanding (length succ-nodes) succ-nodes *-*)
                        (terpri))
          (iter (for succ-node in succ-nodes)
            (hs::push-hstack succ-node open))
          (when (hs::empty-hstack open)  ;nothing left on open to explore
            #+:ww-debug (when (>= *debug* 1)
                          (lprt 'open-exhausted))
            (increment-global *num-idle-threads*)
            (leave))) ;go back to top
        (increment-global *program-cycles* 1)  ;finished with this cycle
        (setf *average-branching-factor* (compute-average-branching-factor))
        (print-search-progress)))))


#|  ;some suggestions from Claude for improving error handling during multi-threading--evaluate later
(defun gracefully-terminate-all-threads (&key (timeout 5))  ;not needed unless using error handling functions below
  (let ((current-thread sb-thread:*current-thread*))
    (dolist (thread (sb-thread:list-all-threads))
      (unless (eq thread current-thread)
        (sb-thread:interrupt-thread thread
                                    (lambda ()
                                      (sb-thread:abort-thread)))
        (unless (sb-thread:join-thread thread :timeout timeout)
          (sb-thread:terminate-thread thread))))))

(defun process-threads ()  ;process-threads with error handling suggested by Claude, but doesn't seem to matter
  "The main consumer of parallel thread processing with error handling."
  (let ((lparallel:*kernel* nil))
    (unwind-protect
         (progn
           (setf lparallel:*kernel* (lparallel:make-kernel *threads*))
           (let ((channel (lparallel:make-channel))
                 (problems (lparallel.queue:make-queue))
                 (first (lparallel.queue:make-queue)))
             (setf *num-idle-threads* *threads*)
             (handler-case
                 (progn
                   (iter (for i from 1 to *threads*)
                     (lparallel:submit-task channel #'search-parallel problems first))
                   (lparallel.queue:push-queue *open* problems)
                   (iter (sleep 0.1)
                         (when (= *num-idle-threads* *threads*)
                           (leave))
                         (when (eq (lparallel.queue:peek-queue problems) 'error)
                           (error "Error detected in worker thread")))
                   (lparallel.queue:push-queue 'stop problems))
               (error (e)
                 (format t "An error occurred: ~A~%" e)
                 (gracefully-terminate-all-threads)))))
      (when lparallel:*kernel*
        (lparallel:end-kernel :wait t)))))

(defun search-parallel (problems first)  ;search-parallel with error handling but doesn't seem to matter
  "Branch & Bound DFS parallel search in each thread with error handling."
  (handler-case
    (iter
      (let ((open (lparallel.queue:pop-queue problems)))
        (when (eql open 'stop)
          (lparallel.queue:push-queue 'stop problems)
          (return-from search-parallel))
        (increment-global *num-idle-threads* -1)
        #+:ww-debug (when (>= *debug* 1)
                      (let ((*package* (find-package :hs)))
                        (lprt *-* 'entering open)))
        (iter
          (when (lparallel.queue:peek-queue first)
            #+:ww-debug (when (>= *debug* 1)
                          (lprt 'interrupted))
            (increment-global *num-idle-threads*)
            (leave))
          (when (and (> (hs::length-hstack open) 1)
                     (> *num-idle-threads* 0))
            (let ((subopen (split-off open)))
              (when subopen
                #+:ww-debug (when (>= *debug* 1)
                              (let ((*package* (find-package :hs)))
                                (lprt 'splitting subopen open)))
                (lparallel.queue:push-queue subopen problems))))
          (let ((current-node (hs::peek-hstack open))
                (succ-nodes (df-bnb1 open)))
            (declare (ignorable current-node))
            (when (= *program-cycles* 0)
              (when (>= *branch* 0)
                (format t "~&Exploring only branch ~D of ~D~%" *branch* (length succ-nodes))
                (setf succ-nodes (subseq succ-nodes *branch* (1+ *branch*))))
              (setf *num-init-successors* (length succ-nodes))
              (setf *rem-init-successors* (reverse succ-nodes)))
            #+:ww-debug (when (>= *debug* 1)
                          (lprt current-node))
            (when (equal succ-nodes '(first))
              #+:ww-debug (when (>= *debug* 1)
                            (lprt 'first-solution-found))
              (lparallel.queue:push-queue 'found first)
              (increment-global *num-idle-threads*)
              (leave))
            (when succ-nodes
              (if (fboundp 'heuristic?)
                  (setf succ-nodes
                        (sort (copy-list succ-nodes) #'>
                              :key (lambda (node)
                                     (problem-state.heuristic (node.state node)))))
                  (when *randomize-search*
                    (setf succ-nodes (alexandria:shuffle succ-nodes)))))
            #+:ww-debug (when (>= *debug* 1)
                          (lprt 'expanding (length succ-nodes) succ-nodes *-*)
                          (terpri))
            (iter (for succ-node in succ-nodes)
              (hs::push-hstack succ-node open))
            (when (hs::empty-hstack open)
              #+:ww-debug (when (>= *debug* 1)
                            (lprt 'open-exhausted))
              (increment-global *num-idle-threads*)
              (leave)))
          (increment-global *program-cycles* 1)
          (setf *average-branching-factor* (compute-average-branching-factor))
          (print-search-progress))))
    (error (e)
      (format *error-output* "Error in search-parallel: ~A~%" e)
      (increment-global *num-idle-threads*)
      (lparallel.queue:push-queue 'error problems)
      (values nil e))))
|#

#|  ;some chatGPT suggestions for improving error handling--evaluate later
(defparameter *worker-threads* nil "List of worker threads.")
(defparameter *terminate-flag* nil "Flag to signal thread termination.")
(defparameter *thread-lock* (sb-thread:make-lock "Thread Lock"))

(defun increment-global (var amount)
  (sb-thread:with-lock-held (*thread-lock*)
    (incf var amount)))

(defun gracefully-terminate-worker-threads (&key (timeout 5))
  "Gracefully terminate all managed worker threads."
  (setf *terminate-flag* t)
  (dolist (thread *worker-threads*)
    (when (and thread (sb-thread:thread-alive-p thread))
      ;; Optionally, signal threads via condition variables or other mechanisms
      ))
  ;; Wait for threads to terminate gracefully
  (dolist (thread *worker-threads*)
    (when (and thread (sb-thread:thread-alive-p thread))
      (unless (sb-thread:join-thread thread :timeout timeout)
        ;; Force termination if graceful termination fails
        (sb-thread:terminate-thread thread))))
  (setf *worker-threads* nil)
  (setf *terminate-flag* nil))

(defun process-threads ()
  "The main consumer of parallel thread processing with enhanced error handling."
  (let ((lparallel:*kernel* nil))
    (unwind-protect
         (progn
           (setf lparallel:*kernel* (lparallel:make-kernel *threads*))
           (let ((channel (lparallel:make-channel))
                 (problems (lparallel.queue:make-queue))
                 (first (lparallel.queue:make-queue)))
             (setf *num-idle-threads* *threads*)
             (handler-case
                 (progn
                   (dotimes (_ *threads*)
                     (let ((thread (lparallel:submit-task channel #'search-parallel problems first)))
                       (push thread *worker-threads*)))
                   (lparallel.queue:push-queue *open* problems)
                   (loop
                      (sleep 0.1)
                      (cond
                        ((= *num-idle-threads* *threads*)
                         (return))
                        ((eq (lparallel.queue:peek-queue problems) 'error)
                         (error "Error detected in worker thread"))))
                   (lparallel.queue:push-queue 'stop problems))
               (error (e)
                 (format t "An error occurred: ~A~%" e)
                 (gracefully-terminate-worker-threads))))
          (when lparallel:*kernel*
            (lparallel:end-kernel :wait t)
            (gracefully-terminate-worker-threads)))))

(defun search-parallel (problems first)
  "Branch & Bound DFS parallel search in each thread with improved error handling."
  (handler-case
      (loop
         (if *terminate-flag*
             (return))
         (let ((open (lparallel.queue:pop-queue problems)))
           (cond
             ((eql open 'stop)
              (lparallel.queue:push-queue 'stop problems)
              (return))
             (t
              (increment-global *num-idle-threads* -1)
              ;; Processing logic here
              ;; ...
              ))))
    (error (e)
      (format *error-output* "Error in search-parallel: ~A~%" e)
      (increment-global *num-idle-threads*)
      (lparallel.queue:push-queue 'error problems)
      (return))))
|#

(defun split-off (open)
  "Removes the bottom node on open and returns a new split-off subopen with it."
  (let ((subopen (hs::make-hstack :table (make-hash-table :test (if (fixedp *relations*)
                                                                  'fixed-keys-ht-equal
                                                                  'equalp)
                                                          :rehash-size 2.0
                                                          :rehash-threshold 0.8
                                                          :synchronized (> *threads* 0))
                                  :keyfn (hs::hstack.keyfn open)))
        (bottom-node (hs::deletef-nth-hstack 0 open)))  ;pops bottom node from open
    (hs::push-hstack bottom-node subopen)))
