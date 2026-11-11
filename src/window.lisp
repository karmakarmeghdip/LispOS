;;;; src/window.lisp - Tiling Window Tree and Unified Modeline for LispOS

(in-package :lispos.window)

(defstruct window
  (id (gensym "WIN-") :type symbol)
  (buffer nil)                        ; active lispos.buffer:buffer instance
  (x 0 :type fixnum)
  (y 0 :type fixnum)
  (width 1280 :type fixnum)
  (height 800 :type fixnum)
  (scroll-row 0 :type fixnum)
  (scroll-col 0 :type fixnum)
  ;; Tree layout slots
  (split-type nil :type (or null (member :horizontal :vertical)))
  (split-ratio 0.5 :type single-float)
  (first-child nil)
  (second-child nil)
  (parent nil))

(defvar *root-window* nil
  "Root window of the display tiling tree.")

(defvar *focused-window* nil
  "Currently focused leaf window receiving input.")

(defun leaf-window-p (win)
  "Return T if WIN is a leaf window displaying a buffer."
  (null (window-split-type win)))

(defun all-leaf-windows (&optional (win *root-window*))
  "Collect all active leaf windows in display order."
  (when win
    (if (leaf-window-p win)
        (list win)
        (append (all-leaf-windows (window-first-child win))
                (all-leaf-windows (window-second-child win))))))

(defun layout-windows (win x y w h)
  "Recursively recalculate (x, y, width, height) bounds for WIN and all its child windows."
  (setf (window-x win) x
        (window-y win) y
        (window-width win) w
        (window-height win) h)
  (let ((split (window-split-type win))
        (ratio (window-split-ratio win)))
    (cond
      ((eq split :vertical)
       ;; Side-by-side split (vertical divider)
       (let* ((w1 (round (* w ratio)))
              (w2 (- w w1)))
         (when (window-first-child win)
           (layout-windows (window-first-child win) x y w1 h))
         (when (window-second-child win)
           (layout-windows (window-second-child win) (+ x w1) y w2 h))))

      ((eq split :horizontal)
       ;; Top-and-bottom split (horizontal divider)
       (let* ((h1 (round (* h ratio)))
              (h2 (- h h1)))
         (when (window-first-child win)
           (layout-windows (window-first-child win) x y w h1))
         (when (window-second-child win)
           (layout-windows (window-second-child win) x (+ y h1) w h2)))))))

(defun split-window-vertically (&optional (win *focused-window*))
  "Split WIN side-by-side into two equal vertical tiles."
  (let ((target (or win *focused-window*)))
    (when (and target (leaf-window-p target))
      (let* ((orig-buf (window-buffer target))
             ;; Left child keeps original buffer
             (child1 (make-window :buffer orig-buf :parent target))
             ;; Right child displays a scratch buffer or second buffer
             (other-buf (or (find-if (lambda (b) (not (eq b orig-buf))) (lispos.buffer:list-buffers))
                            (lispos.buffer:create-buffer "*scratch*" :mode :scratch)))
             (child2 (make-window :buffer other-buf :parent target)))
        (setf (window-split-type target) :vertical
              (window-split-ratio target) 0.5
              (window-first-child target) child1
              (window-second-child target) child2
              (window-buffer target) nil
              *focused-window* child2)
        (layout-windows *root-window*
                        (window-x *root-window*)
                        (window-y *root-window*)
                        (window-width *root-window*)
                        (window-height *root-window*))
        child2))))

(defun split-window-horizontally (&optional (win *focused-window*))
  "Split WIN top-and-bottom into two equal horizontal tiles."
  (let ((target (or win *focused-window*)))
    (when (and target (leaf-window-p target))
      (let* ((orig-buf (window-buffer target))
             (child1 (make-window :buffer orig-buf :parent target))
             (other-buf (or (find-if (lambda (b) (not (eq b orig-buf))) (lispos.buffer:list-buffers))
                            (lispos.buffer:create-buffer "*scratch*" :mode :scratch)))
             (child2 (make-window :buffer other-buf :parent target)))
        (setf (window-split-type target) :horizontal
              (window-split-ratio target) 0.5
              (window-first-child target) child1
              (window-second-child target) child2
              (window-buffer target) nil
              *focused-window* child2)
        (layout-windows *root-window*
                        (window-x *root-window*)
                        (window-y *root-window*)
                        (window-width *root-window*)
                        (window-height *root-window*))
        child2))))

(defun close-window (&optional (win *focused-window*))
  "Close leaf window WIN and restore remaining sibling to parent node."
  (let ((target (or win *focused-window*)))
    (when (and target (leaf-window-p target) (window-parent target))
      (let* ((parent (window-parent target))
             (sibling (if (eq (window-first-child parent) target)
                          (window-second-child parent)
                          (window-first-child parent))))
        ;; Collapse parent into sibling's properties
        (setf (window-split-type parent) (window-split-type sibling)
              (window-split-ratio parent) (window-split-ratio sibling)
              (window-buffer parent) (window-buffer sibling)
              (window-first-child parent) (window-first-child sibling)
              (window-second-child parent) (window-second-child sibling))
        (when (window-first-child parent)
          (setf (window-parent (window-first-child parent)) parent))
        (when (window-second-child parent)
          (setf (window-parent (window-second-child parent)) parent))
        (setf *focused-window* (first (all-leaf-windows parent)))
        (layout-windows *root-window*
                        (window-x *root-window*)
                        (window-y *root-window*)
                        (window-width *root-window*)
                        (window-height *root-window*))
        t))))

(defun focus-window (win)
  "Set focused window to WIN."
  (when (and win (leaf-window-p win))
    (setf *focused-window* win
          (lispos.buffer:*current-buffer*) (window-buffer win))))

(defun focus-direction (dir)
  "Directionally switch window focus (:left, :right, :up, :down, or :next)."
  (let ((leaves (all-leaf-windows))
        (cur *focused-window*))
    (when (and cur (> (length leaves) 1))
      (case dir
        (:next
         (let* ((pos (position cur leaves))
                (next-pos (mod (1+ (or pos 0)) (length leaves))))
           (focus-window (nth next-pos leaves))))
        (:prev
         (let* ((pos (position cur leaves))
                (prev-pos (mod (1- (or pos 0)) (length leaves))))
           (focus-window (nth prev-pos leaves))))
        (:left
         (let ((best (loop for w in leaves
                           when (< (+ (window-x w) (window-width w)) (+ (window-x cur) 5))
                           collect w)))
           (when best (focus-window (first (sort best #'> :key #'window-x))))))
        (:right
         (let ((best (loop for w in leaves
                           when (> (window-x w) (- (+ (window-x cur) (window-width cur)) 5))
                           collect w)))
           (when best (focus-window (first (sort best #'< :key #'window-x))))))
        (:up
         (let ((best (loop for w in leaves
                           when (< (+ (window-y w) (window-height w)) (+ (window-y cur) 5))
                           collect w)))
           (when best (focus-window (first (sort best #'> :key #'window-y))))))
        (:down
         (let ((best (loop for w in leaves
                           when (> (window-y w) (- (+ (window-y cur) (window-height cur)) 5))
                           collect w)))
           (when best (focus-window (first (sort best #'< :key #'window-y))))))))))

(defun active-window-modeline-info ()
  "Generate unified status line information for active window and system."
  (let* ((win (or *focused-window* (first (all-leaf-windows))))
         (buf (and win (window-buffer win)))
         (buf-name (if buf (lispos.buffer:buffer-name buf) "*none*"))
         (mode-str (if buf (string-upcase (symbol-name (lispos.buffer:buffer-mode buf))) "NORMAL"))
         (pkg (package-name *package*)))
    (multiple-value-bind (row col)
        (if buf (lispos.buffer:buffer-cursor-position buf) (values 0 0))
      (format nil "[ ~A ]  ~A   •   ~A   │   Ln ~D, Col ~D   │   Super+b: Buffers   Super+f: Files   Super+v/s: Split"
              mode-str buf-name pkg (1+ row) (1+ col)))))
