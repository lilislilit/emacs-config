;;(setq max-lisp-eval-depth (* 1024 1024))

(transient-mark-mode 1)
(menu-bar-mode 0)
(tool-bar-mode 0)
(toggle-scroll-bar -1)
(column-number-mode 1)

(setq inhibit-startup-screen t
      x-select-enable-clipboard t
      frame-title-format "%F"
      default-input-method 'russian-computer
      lpr-command "xpp")

(put 'scroll-left 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(require 'warnings)
(add-to-list 'warning-suppress-types '(undo discard-info))

(add-to-list 'load-path (expand-file-name "~/.emacs.d/loadable/"))

(when (< emacs-major-version 23)
  (setq user-emacs-directory "~/.emacs.d/"))

(setq my-emacs-var-dir "~/.emacs.var"
      my-emacs-personal-cfg (expand-file-name "personal" user-emacs-directory)
      auto-save-list-file-prefix (expand-file-name "auto-save/" my-emacs-var-dir))

(defun load-ext (cfg &optional name)

  (defun do-load (cfg)
    (load (expand-file-name (format "cfg/%s.el" cfg)
			    user-emacs-directory))
    (let ((path (expand-file-name (concat cfg ".el")
				  my-emacs-personal-cfg)))
      (if (file-regular-p path)
          (load path)
        nil)))

  (if name
      (if (symbolp name)
	  (if (fboundp name)
	      (do-load cfg))
	(if (find-if #'(lambda (path)
			 (find-if #'(lambda (dir)
				      (equal 0 (search name dir)))
				  (nreverse (split-string
					     (directory-file-name path) "/"))))
		     load-path)
	    (do-load cfg)))
    
    (do-load cfg)))

(let ((opt-site-lisp (expand-file-name "~/opt/share/emacs/site-lisp")))
  (when (file-directory-p opt-site-lisp)
    (add-to-list 'load-path opt-site-lisp)
    (dolist (entry (delete nil
			   (mapcar #'(lambda (entry)
				       (unless (or (string= entry ".") (string= entry ".."))
					 (expand-file-name entry opt-site-lisp)))
				   (directory-files opt-site-lisp nil nil t))))
      (when (file-directory-p entry)
	(add-to-list 'load-path entry)))))

;;-------------------------------------------------------------------------------

(require 'lisp-mode)
(define-key lisp-mode-shared-map (kbd "M-t") 'lisp-complete-symbol)

;;-------------------------------------------------------------------------------

;; iswitchb
(require 'iswitchb)
(setq iswitchb-regexp t)
(iswitchb-mode 1)

;;-------------------------------------------------------------------------------

(require 'whitespace)
(global-whitespace-mode 0)
(setq
 whitespace-style '(indentation space-before-tab
                                space-after-tab))

;;-------------------------------------------------------------------------------

(require 'calculator)
(setq calculator-electric-mode nil)

;;-------------------------------------------------------------------------------

(require 'compile)
(setq compilation-error-regexp-metaware
      '("[Ew] \"\\(.*\\)\",L\\([0-9]+\\)/C\\([0-9]+\\)\\((#[0-9]+)\\)?:\t\
\\(.*\\)"
        1 2 3))
(setq compilation-error-regexp-alist
      `(,compilation-error-regexp-metaware gnu gcc-include))

;;-------------------------------------------------------------------------------

(require 'epa)
(setenv "GPG_AGENT_INFO" nil)

;;-------------------------------------------------------------------------------

(require 'ediff)

(setq ediff-window-setup-function 'ediff-setup-windows-plain
      ediff-split-window-function #'(lambda (&optional arg)
                                      (if (> (frame-width) 140)
                                          (split-window-horizontally arg)
                                        (split-window-vertically arg))))

;;-------------------------------------------------------------------------------

(require 'cperl-mode)

(fset 'perl-mode 'cperl-mode)

(add-hook 'cperl-mode-hook
          (lambda ()
            (cperl-set-style "C++")))

;;-------------------------------------------------------------------------------

(require 'cl)
(setq auto-mode-alist (acons ".*\\.bb$" 'shell-script-mode auto-mode-alist))
(setq auto-mode-alist (acons "Makefile\\..*" 'makefile-mode auto-mode-alist))
(setq auto-mode-alist (acons ".*\\.h\\.in$" 'c-mode auto-mode-alist))
(setq auto-mode-alist (acons ".*\\.bat$" 'dos-mode auto-mode-alist))

;;-------------------------------------------------------------------------------

(require 'shell)
(defun shell-jump ()
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
    (unless (get-buffer new-shell-buf-name)
      (add-to-list 'same-window-buffer-names new-shell-buf-name))
    (shell new-shell-buf-name )))

;;-------------------------------------------------------------------------------

(setq dired-bind-jump nil)
(require 'dired-x)

(defadvice dired-do-shell-command
  (around split-fashion (command &optional arg file-list) 
	  activate)
  "Controls the fashion of window splitting. Splits window
vertically."
  (let ((split-height-threshold 0)
	(split-width-threshold nil))
    ad-do-it))

(defadvice dired-do-async-shell-command
  (around split-fashion (command &optional arg file-list) 
	  activate)
  "Controls the fashion of window splitting. Splits window
vertically."
  (let ((split-height-threshold 0)
	(split-width-threshold nil))
    ad-do-it))

(define-key dired-mode-map (kbd "M-n") #'dired-next-line)
(define-key dired-mode-map (kbd "M-p") #'dired-previous-line)
(define-key dired-mode-map (kbd "c") #'dired-do-copy)
(define-key dired-mode-map (kbd "d") #'dired-do-delete)
(define-key dired-mode-map (kbd "M-d") #'dired-flag-file-deletion)
(define-key dired-mode-map (kbd "g") #'dired-do-redisplay)
(define-key dired-mode-map (kbd "l") #'dired-do-relsymlink)
(define-key dired-mode-map (kbd "M-l") #'dired-do-hardlink)
(define-key dired-mode-map (kbd "j") #'dired-find-file)
(define-key dired-mode-map (kbd "M-j") #'dired-goto-file)
(define-key dired-mode-map (kbd "r") #'revert-buffer)

;;-------------------------------------------------------------------------------

(let ((elpa-root (expand-file-name "~/elisp/elpa")))
  (when (file-directory-p elpa-root)
    (add-to-list 'load-path elpa-root)
    (setf package-user-dir elpa-root)
    (require 'package)
    (package-initialize)))

;;-------------------------------------------------------------------------------

;; external packages or big configuration statements should be moved
;; to separate file
(load-ext "server")
(load-ext "cc")
(load-ext "fonts")
(load-ext "env")
(load-ext "org" "org")
(load-ext "gtags" "global")
(load-ext "prefs" "prefs")
(load-ext "zencolor" "zenburn")
(load-ext "fs")
(load-ext "grep")
(load-ext "psvn" 'svn-status)
(load-ext "sr" "sunrise")
(load-ext "iresize" "iresize")
(load-ext "wn" "window-numbering")
(load-ext "ac" "auto-complete")

(when (not (string= system-type "windows-nt"))
  (load-ext "mpc" 'mpc)
  (load-ext "dictem" "dictem")
  (load-ext "nt" "newsticker")
  (load-ext "jabber" "emacs-jabber")
  (load-ext "wl" "wl")
  (load-ext "magit" "magit")
  (load-ext "w3m" "w3m")
  (load-ext "gdb"))

;;-------------------------------------------------------------------------------

(load "my-utils")
(load "buffer-recode")
(load "wm")
(ring-insert evm-coding-systems-list 'windows-1251)
(ring-insert evm-coding-systems-list 'koi8-r)
(global-set-key [f5] 'recode-buffer)

;;-------------------------------------------------------------------------------
;; loaded finally to be sure that all mode maps are available

(load-ext "keys")

;;-------------------------------------------------------------------------------

(when (not (string= system-type "windows-nt"))
  (add-to-list 'initial-frame-alist '(name . "emacs-initial"))
  (add-to-list 'default-frame-alist '(name . "emacs-client"))
  (add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))
  (when (functionp 'wl)
    (wl 1)))
