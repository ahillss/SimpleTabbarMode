;;; simple-tabbar-mode.el --- a simple tabbar mode

;; Author: andrew hills
;; URL:         https://github.com/andrewhills/emacs
;; Version:     1.0

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

(make-variable-buffer-local 'simple-tabbar-scroll)

(defface simple-tabbar-mode-default
  '((((type tty)) :foreground "white" :background "black")
    (t :foreground "black" :background "grey80" ))
  "" :group 'simple-tabbar)

(defface simple-tabbar-mode-selected
  '((((type tty)) :inherit simple-tabbar-mode-default :foreground "yellow")
    (t :inherit simple-tabbar-mode-default :foreground "black"
       :background "white"))
  "" :group 'simple-tabbar)

(defface simple-tabbar-mode-disabled
  '((((type tty)) :inherit simple-tabbar-mode-default :foreground "blue")
    (t :inherit simple-tabbar-mode-default :foreground "grey60"))
  "" :group 'simple-tabbar)

(defface simple-tabbar-mode-separator
  '((t (:height 0.1 )))
  "" :group 'simple-tabbar)

(defun simple-tabbar-buffer-compare (a b)
  (let ((a-file-name (buffer-file-name a))
        (b-file-name (buffer-file-name b)))
    (and a-file-name b-file-name
         (string< (downcase a-file-name)
                  (downcase b-file-name)))))

(defun simple-tabbar-buffer-filter (x)
  (and (buffer-file-name x) x))

(defun simple-tabbar-list-buffers ()
  (sort (delq nil (mapcar 'simple-tabbar-buffer-filter (buffer-list)))
        'simple-tabbar-buffer-compare))

(defun simple-tabbar-prev ()
  (interactive)
  (setq simple-tabbar-scroll nil)
  (let ((bufs (simple-tabbar-list-buffers))
        (curbuf (current-buffer)))
    (while bufs
      (when (and (cdr bufs) (eq curbuf (cadr bufs)))
        (switch-to-buffer (car bufs))
        (setq bufs nil))
      (setq bufs (cdr bufs)))))

(defun simple-tabbar-next ()
  (interactive)
  (setq simple-tabbar-scroll nil)
  (let ((bufs (simple-tabbar-list-buffers))
        (curbuf (current-buffer)))
    (while bufs
      (when (and (cdr bufs) (eq curbuf (car bufs)))
        (switch-to-buffer (cadr bufs))
        (setq bufs nil))
      (setq bufs (cdr bufs)))))

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
             (progn
               (setq simple-tabbar-scroll nil)
               (kill-buffer buf)))
            ((and (bufferp buf) (eq 'mouse-1 m))
             (progn
               (setq simple-tabbar-scroll nil)
               (switch-to-buffer buf)))))))

(defvar simple-tabbar-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km [header-line down-mouse-1] 'simple-tabbar-mode-click)
    (define-key km [header-line down-mouse-3] 'simple-tabbar-mode-click)
    (define-key km [header-line mouse-3] 'ignore)
    km))

(defun simple-tabbar-tab-name (buf)
  (concat " " (buffer-name buf)
          (if (and (buffer-file-name buf)
                   (buffer-modified-p buf))
              "*" " ")))

(defun simple-tabbar-tab-tooltip (buf)
  (let ((fn (buffer-file-name buf)))
    (if fn fn (buffer-name buf))
    ))

(defvar simple-tabbar-scroll-left-prop
  (propertize
   " < " 'face 'simple-tabbar-mode-default
   'local-map simple-tabbar-mode-map 'buf 'prev))

(defvar simple-tabbar-scroll-left-disabled-prop
  (propertize
   " < " 'face 'simple-tabbar-mode-disabled
   'local-map simple-tabbar-mode-map 'buf nil))

(defvar simple-tabbar-scroll-right-prop
  (propertize
   " > " 'face 'simple-tabbar-mode-default
   'local-map simple-tabbar-mode-map 'buf 'next))

(defvar simple-tabbar-scroll-right-disabled-prop
  (propertize
   " > " 'face 'simple-tabbar-mode-disabled
   'local-map simple-tabbar-mode-map 'buf nil))

(defvar simple-tabbar-space-prop
  (propertize
   " " 'face 'simple-tabbar-mode-separator
   'local-map simple-tabbar-mode-map 'buf nil))

(defvar simple-tabbar-scrolls-width
  (+ (if (> (string-width  simple-tabbar-scroll-left-prop)
            (string-width simple-tabbar-scroll-left-disabled-prop))
         (string-width  simple-tabbar-scroll-left-prop)
       (string-width simple-tabbar-scroll-left-disabled-prop))
     (if (> (string-width simple-tabbar-scroll-right-prop)
            (string-width simple-tabbar-scroll-right-disabled-prop))
         (string-width simple-tabbar-scroll-right-prop)
       (string-width simple-tabbar-scroll-right-disabled-prop)) ))

(defun simple-tabbar-mode-tabs ()
  (let ((width (- (window-total-width) simple-tabbar-scrolls-width))
        (bufs (simple-tabbar-list-buffers))
        (scroll-rightable nil)
        (result nil)
        (pos 0)
        (scrolls 0)
        (curbuf (current-buffer)))
    (while bufs
      (let* ((buf (car bufs))
             (face (if (eq buf curbuf)
                       'simple-tabbar-mode-selected
                     'simple-tabbar-mode-default))
             (tab-prop
              (propertize
               (simple-tabbar-tab-name buf) 'face face
               'local-map simple-tabbar-mode-map 'buf buf
               'help-echo (simple-tabbar-tab-tooltip buf)
               ))
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
          (setq bufs nil))

        ))

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

(defvar simple-tabbar-mode-header-line-format-old nil)

(defvar simple-tabbar-mode-tabs-function 'simple-tabbar-mode-tabs)

(defconst simple-tabbar-mode-header-line-format
  '(:eval (funcall simple-tabbar-mode-tabs-function)))

(define-minor-mode simple-tabbar-mode
  "desc"
  nil nil
  simple-tabbar-mode-map
  :group 'simple-tabbar-mode
  (if simple-tabbar-mode
      (progn
        (when (and (local-variable-p 'header-line-format)
		   (not (local-variable-p
                         'simple-tabbar-mode-header-line-format-old)))
          (set (make-local-variable
                'simple-tabbar-mode-header-line-format-old)
               header-line-format))
        (setq header-line-format simple-tabbar-mode-header-line-format)
        (add-hook 'post-command-hook 'force-mode-line-update nil t))
    (when (eq header-line-format simple-tabbar-mode-header-line-format)
      (kill-local-variable 'header-line-format)
      (when (local-variable-p 'simple-tabbar-mode-header-line-format-old)
        (setq header-line-format simple-tabbar-mode-header-line-format-old)
        (kill-local-variable 'simple-tabbar-mode-header-line-format-old)))
    (remove-hook 'post-command-hook 'force-mode-line-update t)))

;; (defun simple-tabbar-kill-buffer-hook ()
;;   (when (buffer-file-name)))

(add-hook 'find-file-hook 'simple-tabbar-mode)
;; (add-hook 'kill-buffer-hook 'simple-tabbar-kill-buffer-hook)

(add-hook 'window-configuration-change-hook
  '(lambda () (setq simple-tabbar-scroll nil)))

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
