;;
;;
;; rays.el -
;;
;; $Id$
;;
;;------------------------------------------------------------------------------
;; Copyright (c) 2004 Dmitri Priimak
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.
(provide 'ray)
(require 'cl)

(defun rays-quit ()
  (interactive)
  (kill-buffer (current-buffer)))

(defvar rays-mode-map
  (make-sparse-keymap 'rays-mode-map))

(defvar rays-buffer-saved-window-configuration nil
  "The window configuration before the rays help was displayed.")
(defvar rays-x 1)
(defvar rays-y 1)
(defvar rays-board-x-delta "  ")
(defvar rays-board-max-x 10)
(defvar rays-board-max-y 10)
(defvar rays-bombs-total 9)
(defvar bombs (make-hash-table))
(defvar rays-steps 0)
(defvar rays-uncovered 0)

(defface red
  `((t (:background "red")))
  "red face")

(define-minor-mode rays-help-mode
  "Rays Help Mode"
  nil
  " Rays-Help"
  '(("q" . undefined)))

(defun rays-quit-help-buffer()
  (interactive)
  (kill-buffer (current-buffer))
  (set-window-configuration rays-buffer-saved-window-configuration))

(defun rays-show-help ()
  (interactive)
  (let ((buf "*Rays Help*")
        (old-window (selected-window)))
    (if (not (get-buffer buf))
        (setq rays-buffer-saved-window-configuration (current-window-configuration)))
    (get-buffer-create buf)
    (if (get-buffer-window buf)
        (select-window (get-buffer-window buf))
      (progn (split-window-vertically)
             (select-window (next-window))))
    (switch-to-buffer buf)
    (rays-help-mode 1)
    (erase-buffer)
    (insert "Press 'q' to close this buffer\n
The point of 'The Game of Rays' is to uncover all hidden elements (treasures) in
as small amount of steps as possible. There are 9 hidden treasures in a 10 by 10 field.
Use arrows to navigate the field. Press space-bar to uncover field. If this field has
treasure it will change its background color to red. Content of the field will also
show how many elements (hidden and/or uncovered) are within vertical and horizontal
lines intersecting at this field, as if rays been used to locate elements. Hence the
name of the game. To indicate these rays dots on these lines will change to crosses
(plus signs). You can also toggle mark '#' on yet uncovered field by pressing Enter.
This will not affect steps counter. You can use that mark to help yourself to 
remember that certain field does not have any hidden treasures. If latter one you
decided that you made mistake you can un-toggle by pressing Enter key once again.")
    (beginning-of-buffer)))

(defvar rays-help-mode-map)
(define-key rays-help-mode-map "q" 'rays-quit-help-buffer)

(defun rays-linear-to-xy (addr)
  (let* ((addr (1- addr))
         (y (/ addr rays-board-max-x))
         (x (1+ (- addr (* y rays-board-max-x)))))
    (list x (1+ y))))

(defun rays-xy-to-linear (x y)
  (+ x (* (1- y) rays-board-max-y)))

(defun rays-position ()
  (rays-get-position rays-x rays-y))

(defun rays-move-right (&optional free)
  (interactive)
  (if (or free (< rays-x rays-board-max-x))
      (progn (goto-char (1- (re-search-forward " [\\.#\\*\\+0123456789]")))
             (if (< rays-x rays-board-max-x)
                 (incf rays-x)
               (progn (setq rays-x 1)
                      (incf rays-y))))))

(defun rays-move-left (&optional free)
  (interactive)
  (if (or free (> rays-x 1))
      (progn (goto-char (1+ (re-search-backward " [\\.#\\*\\+0123456789]")))
             (if (> rays-x 1)
                 (setq rays-x (1- rays-x))
               (progn (setq rays-x rays-board-max-x)
                      (setq rays-y (1- rays-y)))))))

(defun rays-move-down ()
  (interactive)
  (if (< rays-y rays-board-max-y)
      (let ((saved-x rays-x))
        (do ((i 0 (1+ i)))
            ((= i rays-board-max-x))
          (rays-move-right 't)))))

(defun rays-move-up ()
  (interactive)
  (if (> rays-y 1)
      (let ((saved-x rays-x))
        (do ((i 0 (1+ i)))
            ((= i rays-board-max-x))
          (rays-move-left 't)))))

(suppress-keymap rays-mode-map)
(define-key rays-mode-map "q"      'rays-quit)
(define-key rays-mode-map "n"      'rays)
(define-key rays-mode-map "h"      'rays-show-help)
(define-key rays-mode-map [left]   'rays-move-left)
(define-key rays-mode-map [right]  'rays-move-right)
(define-key rays-mode-map [up]     'rays-move-up)
(define-key rays-mode-map [down]   'rays-move-down)
(define-key rays-mode-map " "      'rays-fire)
(define-key rays-mode-map [return] 'rays-togle-mark)

(defun rays-mode ()
  "Major mode for game 'Rays'"
  (interactive)
  (kill-all-local-variables)
  (use-local-map rays-mode-map)
  (setq mode-name "Rays")
  (setq major-mode 'rays-mode))

(defun rays ()
  "Game of Rays"
  (interactive)
  (switch-to-buffer "*Rays*")
  (setq buffer-read-only 't)
  (rays-mode)
  (rays-start)
  (rays-draw-board rays-board-max-x rays-board-max-y)
  (setq rays-x 1)
  (setq rays-y 1)
  (goto-char 0)
  (goto-char (1- (search-forward "."))))

(defun rays-draw-board (m n &optional show-bombs)
  (buffer-disable-undo (current-buffer))
  (let ((inhibit-read-only t)
        (i 0)
        (j 0))
    (erase-buffer)
    (let ((linear 1))
      (while (<= (incf j) n)
        (insert rays-board-x-delta rays-board-x-delta)
        (while (<= (incf i) m)
          (if (and show-bombs (gethash linear bombs))
              (insert "*" rays-board-x-delta)
            (insert "." rays-board-x-delta))
          (incf linear))
        (insert "\n")
        (setq i 0)))
    (insert "\n\n" rays-board-x-delta rays-board-x-delta "Steps: 0"
            "\n" rays-board-x-delta rays-board-x-delta "Total Found: 0 out of "
            (number-to-string rays-bombs-total)
            "\n\n" rays-board-x-delta rays-board-x-delta "Commands:"
            "\n" rays-board-x-delta rays-board-x-delta "   'h' to get help"
            "\n" rays-board-x-delta rays-board-x-delta "   'q' to quit"
            "\n" rays-board-x-delta rays-board-x-delta "   'n' to start new game"
            "\n" rays-board-x-delta rays-board-x-delta "   [Space] to uncover field"
	    "\n" rays-board-x-delta rays-board-x-delta "   [Enter] to toggle mark"
            "\n" rays-board-x-delta rays-board-x-delta "   [arrows] to navigate board")))

(defun rays-start ()
  (setq bombs (make-hash-table :size rays-bombs-total))
  (setq rays-steps 0)
  (setq rays-uncovered 0)
  (let ((max-linear (1+ (* rays-board-max-x rays-board-max-y))))
    (do ((linear-addr (1+ (random max-linear))
		      (1+ (random max-linear)))
       (placed 0))
      ((= placed rays-bombs-total))
      (if (null (gethash linear-addr bombs))
	  (progn (puthash linear-addr 'h bombs)
		 (incf placed))))))

(defun rays-goto-xy (x y)
  (let ((linear-addr (rays-xy-to-linear x y)))
    (goto-char 0)
    (do ((i 1 (1+ i)))
        ((= i (1+ linear-addr)))
      (rays-move-right 't))
    (point)))

(defun rays-put-char (x y char &optional do-put)
  (let ((inhibit-read-only t)
        (saved-pos (point))
        (saved-rays-x rays-x)
        (saved-rays-y rays-y)
        (was-cross nil))
    (rays-goto-xy x y)
    (if (or (eq 46 (char-after)) (eq 43 (char-after)) (eq 35 (char-after)))
        (progn (setq was-cross (get-text-property (point) 'base-cross))
               (if (or do-put (not (and (equal char "+") (eq 35 (char-after)))))
                   (progn (delete-char 1)
                          (insert char)
                          (move-to-column (1- (current-column)))))
               (if (or was-cross (equal char "+"))
		   (put-text-property (point) (1+ (point)) 'base-cross '+))))
    (goto-char saved-pos)
    (setq rays-x saved-rays-x)
    (setq rays-y saved-rays-y)))

(defun rays-togle-mark ()
  (interactive)
  (let ((inhibit-read-only t)
        (base-cross (get-text-property (point) 'base-cross)))
    (if (eq 35 (char-after))
	(if base-cross
	    (rays-put-char rays-x rays-y "+" 't)
	  (rays-put-char rays-x rays-y "."))
      (rays-put-char rays-x rays-y "#"))))

(defun rays-fire ()
  (interactive)
  (if (= rays-uncovered 9)
      (beep)
    (let ((inhibit-read-only t)
          (linear-addr (rays-xy-to-linear rays-x rays-y))
          (located-total 0))
      (do ((i 1 (1+ i)))
          ((= (1- i) rays-board-max-x))
        (rays-put-char i rays-y "+")
        (if (not (null (gethash (rays-xy-to-linear i rays-y) bombs)))
              (incf located-total)))
      (do ((j 1 (1+ j)))
          ((= (1- j) rays-board-max-y))
        (rays-put-char rays-x j "+")
        (if (not (null (gethash (rays-xy-to-linear rays-x j) bombs)))
            (incf located-total)))
      (delete-char 1)
      (insert (number-to-string located-total))
      (move-to-column (1- (current-column)))
      (if (gethash (rays-xy-to-linear rays-x rays-y) bombs)
          (progn (setq located-total (1- located-total))
                 (delete-char 1)
                 (insert (number-to-string located-total))
                 (move-to-column (1- (current-column)))
                 (set-text-properties (point) (1+ (point)) '(face red))
                 (incf rays-uncovered)))
      (incf rays-steps)
      (save-excursion
        (goto-char (- (search-forward "Steps") 5))
        (kill-line)
        (insert "Steps: " (number-to-string rays-steps))
        (goto-char (- (search-forward "Total") 5))
        (kill-line)
        (insert "Total Found: " (number-to-string rays-uncovered)
                " out of " (number-to-string rays-bombs-total))))))
