(defmacro* load-package (name &key options after-load required config)
  `(progn
     ,@(mapcar (lambda (option) `(setf ,(car option) ,(cdr option))) options)
     ,(when after-load
        `(eval-after-load ,name
           (lambda ()
             ,@after-load)))
     ,(when config
        `(eval-after-load ,name
           (lambda ()
             (load-config ,config))))
     ,(when required
        `(require ,name nil t))))

(defun load-config (name)

  "it loads configuration that is saved in conf.d and local.d directories"

  (let ((main  (expand-file-name
                (format "conf.d/%s.el" name) user-emacs-directory))
        (local (expand-file-name
                (format "local.d/%s.el" name) user-emacs-directory)))

    (load main)
    (if (file-regular-p local)
        (load local))))

;;-------------------------------------------------------------------------------

(defun shell-jump ()
  "opens the shell in the current directory. Opens new window if
prefix argument is set"

  (interactive)
  (let ((new-shell-buf-name
         (if (eq major-mode 'dired-mode)
             (concat (buffer-name) ":shell")
           (let* ((dir (directory-file-name
                        (if buffer-file-name
                            (file-name-directory buffer-file-name)
                          default-directory))))
             (concat (car (last (split-string dir "/")))
                     ":shell")))))
    (if current-prefix-arg
        (let ((split-height-threshold 0)
              (split-width-threshold nil))
          (shell new-shell-buf-name))
      (let (pop-up-windows
            (same-window-buffer-names
             (cons new-shell-buf-name same-window-buffer-names)))
        (shell new-shell-buf-name)))))

;;-------------------------------------------------------------------------------

(defun swap-buffers (&optional last)
  "Swaps the current and a last buffers"
  (interactive)
  (let ((buf (current-buffer))
        (frame (selected-frame)))
    (switch-to-buffer
     (if last
         (last-buffer buf t frame)
       (progn
         (other-buffer buf t frame)
         (bury-buffer buf))))))

(provide 'my-utils)

;;-------------------------------------------------------------------------------

 ;;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph
(defun unfill-paragraph (&optional region)
      "Takes a multi-line paragraph and makes it into a single line of text."
      (interactive (progn (barf-if-buffer-read-only) '(t)))
      (let ((fill-column (point-max))
            ;; This would override `fill-column' if it's an integer.
            (emacs-lisp-docstring-fill-column t))
        (fill-paragraph nil region)))
