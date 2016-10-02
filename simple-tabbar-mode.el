;;; simple-tabbar-mode.el --- a simple tabbar mode

;; Author: andrew hills
;; URL:         https://github.com/andrewhills/SimpleTabbarMode
;; Version:     1.02

;;; Commentary:
;; - goto buffer by left clicking the tab
;; - close buffer by right clicking the tab
;; - mouse over tab shows file path
;; - clicking mouse prev/next goes though tabs
;; - top left prev/next tabs scroll through the off screen tabs
;; - C-left, C-right, C-x C-left, C-x C-right navigates the tabs

;;; Usage
;; put (require 'simple-tabbar-mode nil t) in your emacs init file

(defgroup simple-tabbar-mode nil
  "desc" :version "1.0" :group 'convenience)

;;;;;;;;;;;;;;;;;;

(make-variable-buffer-local 'simple-tabbar-scroll)

(defface simple-tabbar-face-default
  '((((type tty)) :background "black" :foreground "white")
    (t :background "grey80" :foreground "black")) "" :group 'simple-tabbar)

(defface simple-tabbar-face-selected
  '((((type tty)) :background "black" :foreground "yellow")
    (t :background "white" :foreground "black")) "" :group 'simple-tabbar)

(defface simple-tabbar-face-disabled
  '((((type tty)) :background "black" :foreground "blue")
    (t :background "grey80" :foreground "grey60")) "" :group 'simple-tabbar)

(defface simple-tabbar-face-other
  '((((type tty)) :background "black" :foreground "white")
    (t :background "grey50" :foreground "black")) "" :group 'simple-tabbar)

(defface simple-tabbar-face-separator
  '((t (:height 0.1))) "" :group 'simple-tabbar)

(defvar simple-tabbar-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km [header-line down-mouse-1] 'simple-tabbar-mode-click)
    (define-key km [header-line down-mouse-3] 'simple-tabbar-mode-click)
    (define-key km [header-line mouse-3] 'ignore)
    km))

(defvar simple-tabbar-scroll-left-prop
  (propertize " < " 'face 'simple-tabbar-face-default
              'local-map simple-tabbar-mode-map 'buf 'prev))

(defvar simple-tabbar-scroll-left-disabled-prop
  (propertize " < " 'face 'simple-tabbar-face-disabled
              'local-map simple-tabbar-mode-map 'buf nil))

(defvar simple-tabbar-scroll-right-prop
  (propertize " > " 'face 'simple-tabbar-face-default
              'local-map simple-tabbar-mode-map 'buf 'next))

(defvar simple-tabbar-scroll-right-disabled-prop
  (propertize " > " 'face 'simple-tabbar-face-disabled
              'local-map simple-tabbar-mode-map 'buf nil))

(defvar simple-tabbar-space-prop
  (propertize " " 'face 'simple-tabbar-face-separator
              'local-map simple-tabbar-mode-map 'buf nil))

(defun simple-tabbar-mode-click (event)
  (interactive "@e")
  (when (memq 'down (event-modifiers event))
    (let* ((target (posn-string (event-start event)))
           (m (event-basic-type event))
           (buf (get-text-property (cdr target) 'buf (car target))))
      (cond ((and (eq 'next buf) (eq 'mouse-1 m))
             (setq simple-tabbar-scroll (+ simple-tabbar-scroll 1)))
            ((and (eq 'prev buf) (eq 'mouse-1 m))
             (setq simple-tabbar-scroll (- simple-tabbar-scroll 1)))
            ((and (bufferp buf) (eq 'mouse-3 m))
             (progn (setq simple-tabbar-scroll nil)
                    (kill-buffer buf)))
            ((and (bufferp buf) (eq 'mouse-1 m))
             (if (buffer-file-name buf)
                 (progn (setq simple-tabbar-scroll nil)
                        (switch-to-buffer buf))
               (switch-to-buffer-other-window buf) ))))))

(defun simple-tabbar-tab-name (buf)
  (let ((fn (buffer-file-name buf))
        (name (buffer-name buf)))
    (concat " " (if fn name (replace-regexp-in-string "\*+" "" name))
            (if (and fn (buffer-modified-p buf)) "*" " "))))

(defun simple-tabbar-tab-tooltip (buf)
  (let ((fn (buffer-file-name buf)))
    (if fn fn (buffer-name buf)) ))

(defun simple-tabbar-buffer-compare (a b)
  (let ((a-fn (buffer-file-name a))
        (b-fn (buffer-file-name b)))
    (cond ((and a-fn b-fn) (string< (downcase a-fn) (downcase b-fn)))
          (a-fn t)
          (b-fn nil)
          (t (string< (downcase (buffer-name a))
                      (downcase (buffer-name b)))) )))

(defun simple-tabbar-ordered-bufs ()
  (sort (delq nil (buffer-list)) 'simple-tabbar-buffer-compare))

(defun simple-tabbar-ordered-bufs-ext ()
  (let* ((a (buffer-list))
         (b (mapcar #'(lambda (x) (if (buffer-file-name x) x nil)) a)))

    (sort (delq nil b) 'simple-tabbar-buffer-compare)))


(defun simple-tabbar-tabs ()
  (let ((width (- (window-total-width)
                  (string-width simple-tabbar-scroll-left-prop)
                  (string-width simple-tabbar-scroll-right-prop)))
        (bufs (simple-tabbar-ordered-bufs))
        (scroll-rightable nil)
        (result nil)
        (pos 0)
        (scrolls 0)
        (curbuf (current-buffer)))
    (while bufs
      (let* ((buf (car bufs))
             (face (cond ((not (buffer-file-name buf)) 'simple-tabbar-face-other)
                         ((eq buf curbuf) 'simple-tabbar-face-selected)
                         (t 'simple-tabbar-face-default) ))
             (tab-prop
              (propertize
               (simple-tabbar-tab-name buf) 'face face
               'local-map simple-tabbar-mode-map 'buf buf
               'help-echo (simple-tabbar-tab-tooltip buf) ))
             (prop-width (string-width tab-prop))
             (move (> (+ prop-width pos)
                      (- width (if (display-graphic-p) 1 0))))
             (scrolls2 (if move (+ 1 scrolls) scrolls)))

        ;;scroll to selected buffer tab
        (when (and (not simple-tabbar-scroll) (eq buf curbuf))
          (setq simple-tabbar-scroll scrolls2))

        ;;discard unseen tabs on left
        (when (and move (or (not simple-tabbar-scroll)
                            (= scrolls2 simple-tabbar-scroll)))
          (setq result nil))

        ;;add tab
        (setq result (cons tab-prop result))

        ;;add space
        (when (display-graphic-p)
          (setq result (cons simple-tabbar-space-prop result)))

        ;;
        (setq pos (if move prop-width (+ pos prop-width)))
        (setq scrolls scrolls2)
        (setq bufs (cdr bufs))
        (setq scroll-rightable
              (and simple-tabbar-scroll
                   (< simple-tabbar-scroll scrolls2)))

        ;;stop when additional tabs will be unseen
        (when (and simple-tabbar-scroll (> scrolls2 simple-tabbar-scroll))
          (setq bufs nil)) ))

    ;;arrange the results the right way around
    (setq result (nreverse result))

    ;;add beginning space
    (when (display-graphic-p)
      (setq result (cons simple-tabbar-space-prop result)))

    ;;add scroll right button
    (if scroll-rightable
        (setq result (cons simple-tabbar-scroll-right-prop result))
      (setq result (cons simple-tabbar-scroll-right-disabled-prop result)))

    ;;add scroll left button
    (if (= 0 simple-tabbar-scroll)
        (setq result (cons simple-tabbar-scroll-left-disabled-prop result))
      (setq result (cons simple-tabbar-scroll-left-prop result)))

    ;;
    result))

;;;;;;;;;;;;;;;

;; (defun simple-tabbar-hook ()
;;   (add-hook 'post-command-hook 'force-mode-line-update nil t)
;;   (setq header-line-format '(:eval (funcall (quote simple-tabbar-tabs)))))

;; (add-hook 'window-configuration-change-hook
;;   #'(lambda () (setq simple-tabbar-scroll nil)))

;; (add-hook 'find-file-hook 'simple-tabbar-hook)

;;;;;;;;;;;;;;;

(defvar simple-tabbar-mode-header-line-format-old nil)

(defvar simple-tabbar-mode-tabs-function 'simple-tabbar-tabs)

(defconst simple-tabbar-mode-header-line-format
  '(:eval (funcall simple-tabbar-mode-tabs-function)))

(define-minor-mode simple-tabbar-mode
  "desc"
  nil nil
  simple-tabbar-mode-map
  :group 'simple-tabbar-mode
  (if simple-tabbar-mode
      (progn
        (when (and (local-variable-p 'header-line-format) (not (local-variable-p 'simple-tabbar-mode-header-line-format-old)))
          (set (make-local-variable 'simple-tabbar-mode-header-line-format-old) header-line-format))
        (setq header-line-format simple-tabbar-mode-header-line-format)
        (add-hook 'post-command-hook 'force-mode-line-update nil t))
    (when (eq header-line-format simple-tabbar-mode-header-line-format)
      (kill-local-variable 'header-line-format)
      (when (local-variable-p 'simple-tabbar-mode-header-line-format-old)
        (setq header-line-format simple-tabbar-mode-header-line-format-old)
        (kill-local-variable 'simple-tabbar-mode-header-line-format-old)))
    (remove-hook 'post-command-hook 'force-mode-line-update t)))

;;;;;;;;;;;;;;;;;;;;;


(defun simple-tabbar-prev ()
  (interactive)
  (setq simple-tabbar-scroll nil)
  (let ((bufs (simple-tabbar-ordered-bufs-ext))
        (curbuf (current-buffer)))
    (while bufs
      (when (and (cdr bufs) (eq curbuf (cadr bufs)))
        (switch-to-buffer (car bufs))
        (setq bufs nil))
      (setq bufs (cdr bufs)))))

(defun simple-tabbar-next ()
  (interactive)
  (setq simple-tabbar-scroll nil)
  (let ((bufs (simple-tabbar-ordered-bufs-ext))
        (curbuf (current-buffer)))
    (while bufs
      (when (and (cdr bufs) (eq curbuf (car bufs)))
        (switch-to-buffer (cadr bufs))
        (setq bufs nil))
      (setq bufs (cdr bufs)))))

;;;;;;;;;;;;;;;;;;;;;


(add-hook 'find-file-hook 'simple-tabbar-mode)
;; (add-hook 'kill-buffer-hook 'simple-tabbar-kill-buffer-hook)

(add-hook 'window-configuration-change-hook
  #'(lambda () (setq simple-tabbar-scroll nil)))

(define-key simple-tabbar-mode-map (kbd "<header-line> <mouse-4>")
  'simple-tabbar-prev)

(define-key simple-tabbar-mode-map (kbd "<header-line> <mouse-5>")
  'simple-tabbar-next)

(define-key simple-tabbar-mode-map (kbd "<header-line> <mouse-8>")
  'simple-tabbar-prev)

(define-key simple-tabbar-mode-map (kbd "<header-line> <mouse-9>")
  'simple-tabbar-next)

(define-key simple-tabbar-mode-map (kbd "C-<left>")
  'simple-tabbar-prev)

(define-key simple-tabbar-mode-map (kbd "C-<right>")
  'simple-tabbar-next)

(define-key simple-tabbar-mode-map (kbd "C-x C-<left>")
  'simple-tabbar-prev)

(define-key simple-tabbar-mode-map (kbd "C-x C-<right>")
  'simple-tabbar-next)

(provide 'simple-tabbar-mode)
