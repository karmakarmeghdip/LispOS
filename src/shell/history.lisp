;;;; src/shell/history.lisp - Command History Ring and Scratch Buffer Retention

(in-package :lispos.shell)

(defun %load-history-entry (engine text)
  "Load TEXT into buffer and place point at the end."
  (%set-buffer-from-string engine text)
  (setf (shell-engine-point engine) (length text)))

(defun handle-history-up (engine)
  "Navigate backwards in command history (Up arrow / C-p).
Non-destructive: retains current unsubmitted line as scratch buffer."
  (let ((history (shell-engine-history engine))
        (h-idx (shell-engine-history-index engine)))
    (when (null history)
      (return-from handle-history-up :no-history))
    (cond
      ;; Not currently browsing history: save scratch and load newest item (index 0)
      ((null h-idx)
       (setf (shell-engine-scratch-buffer engine) (buffer-text engine))
       (setf (shell-engine-scratch-point engine) (shell-engine-point engine))
       (setf (shell-engine-history-index engine) 0)
       (%load-history-entry engine (first history))
       :history-up)
      ;; Already browsing: move to next older item if available
      ((< (1+ h-idx) (length history))
       (incf (shell-engine-history-index engine))
       (%load-history-entry engine (nth (shell-engine-history-index engine) history))
       :history-up)
      (t
       :history-oldest))))

(defun handle-history-down (engine)
  "Navigate forwards in command history (Down arrow / C-n).
Restores scratch buffer when stepping past the newest command."
  (let ((history (shell-engine-history engine))
        (h-idx (shell-engine-history-index engine)))
    (cond
      ((null h-idx)
       :at-scratch)
      ((> h-idx 0)
       (decf (shell-engine-history-index engine))
       (%load-history-entry engine (nth (shell-engine-history-index engine) history))
       :history-down)
      ((= h-idx 0)
       ;; Step past index 0 back to unsubmitted scratch buffer
       (setf (shell-engine-history-index engine) nil)
       (let ((scratch (shell-engine-scratch-buffer engine))
             (s-pt (shell-engine-scratch-point engine)))
         (%set-buffer-from-string engine scratch)
         (setf (shell-engine-point engine) (min (length scratch) s-pt)))
       :restored-scratch))))
