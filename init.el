(require 'cl)
(require 'warnings)

(transient-mark-mode 1)
(menu-bar-mode 0)
(tool-bar-mode 0)
(toggle-scroll-bar -1)
(column-number-mode 1)

(setq
 visible-bell t
 inhibit-startup-screen t
 x-select-enable-clipboard t
 frame-title-format "%F"
 default-input-method 'russian-computer
 lpr-command "xpp")

(put 'scroll-left 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(add-to-list 'warning-suppress-types '(undo discard-info))

(add-to-list 'load-path (expand-file-name "~/.emacs.d/loadable/"))

(require 'config-hacks)

(defvar my-emacs-var-dir "~/.emacs.var" 
  "directory to keep data files generated by the extensions")

(defvar my-emacs-personal-cfg
  (expand-file-name "personal" user-emacs-directory)
  "directory to keep host specific configuration files those
should be pushed to git repo")

(setq auto-save-list-file-prefix
  (expand-file-name "auto-save/" my-emacs-var-dir))

;;-------------------------------------------------------------------------------

(defvar ignored-buffer-list 
  '("*Completions" "*Quail Completions*")
"list of the buffer names or regular expressions to be ignored by
various buffer management routines")

(add-to-list 'ignored-buffer-list "*magit-edit-log*")

(defun suitable-buffer-p (buffer)
  "predicate to check the buffer exclusion from the `ignored-buffer-list'"
  (if (find-if #'(lambda (entry)
		   (string-match entry (buffer-name buffer)))
	       ignored-buffer-list)
      nil
    t))

(add-to-list 'default-frame-alist `(buffer-predicate . ,#'suitable-buffer-p))

;;-------------------------------------------------------------------------------

(defun load-ext (cfg &optional name)

  (defun do-load (cfg)
    (load (expand-file-name (format "cfg/%s.el" cfg)
			    user-emacs-directory))
    (let ((path (expand-file-name (format "%s.el" cfg)
				  my-emacs-personal-cfg)))
      (when (file-regular-p path)
	(load path)))
    t)

  (if name
      (if (symbolp name)
	  (when (fboundp name)
	    (do-load cfg))
	(let ((rexp (format "/%s" name)))
	  (when (find-if #'(lambda (path)
			     (string-match rexp path))
			 load-path)
	    (do-load cfg))))
    
    (do-load cfg)))

;;-------------------------------------------------------------------------------

(let ((elpa-root (expand-file-name "~/elisp/elpa")))
  (when (file-directory-p elpa-root)
    (add-to-list 'load-path elpa-root)
    (setq package-user-dir elpa-root)
    (require 'package)
    (package-initialize)))

;;-------------------------------------------------------------------------------

(let ((opt-site-lisp (expand-file-name "~/opt/share/emacs/site-lisp")))
  (when (file-directory-p opt-site-lisp)
    ;; will add directory itself to path
    (dolist (entry (directory-files opt-site-lisp t nil t))
      (when (and (string-prefix-p opt-site-lisp entry)
		 (file-directory-p entry))
	(add-to-list 'load-path (expand-file-name entry))))))

;;-------------------------------------------------------------------------------

(add-to-list 'auto-mode-alist '(".*\\.bb$" . shell-script-mode))
(add-to-list 'auto-mode-alist '("Makefile\\..*" . makefile-mode))
(add-to-list 'auto-mode-alist '(".*\\.h\\.in$" . c-mode))
(add-to-list 'auto-mode-alist '(".*\\.bat$" . dos-mode))
(add-to-list 'auto-mode-alist '("svn-.*\\.tmp$" . text-mode))

(add-hook 'text-mode-hook 'auto-fill-mode)

;;-------------------------------------------------------------------------------

;; iswitchb
(require 'iswitchb)
(setq iswitchb-regexp t
      iswitchb-default-method 'samewindow)
(iswitchb-mode 1)

(mapcar #'(lambda (entry)
	    (add-to-list 'iswitchb-buffer-ignore entry))
	ignored-buffer-list)

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
	    (local-set-key (kbd "C-h f") 'cperl-perldoc)
	    (setq indent-tabs-mode nil)
            (cperl-set-style "C++")))

;;-------------------------------------------------------------------------------

(require 'shell)
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
      (let ((same-window-buffer-names
	     (cons new-shell-buf-name same-window-buffer-names)))
	(shell new-shell-buf-name)))))

;;-------------------------------------------------------------------------------

(setq dired-bind-jump nil
      dired-recursive-deletes 'always
      dired-deletion-confirmer #'y-or-n-p)
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
(define-key dired-mode-map (kbd "r") #'dired-do-rename)
(define-key dired-mode-map (kbd "M-d") #'dired-flag-file-deletion)

(put 'dired-find-alternate-file 'disabled nil)

;; -l is mandatory
;; -G omit the group name
;; -h human-readable size
(setq dired-listing-switches "-alGh")

;;-------------------------------------------------------------------------------n
(require 'info)

(define-key Info-mode-map (kbd "j") #'Info-follow-nearest-node)
(define-key Info-mode-map (kbd "M-n") nil)

;;-------------------------------------------------------------------------------
(require 'vc)

(add-to-list 'same-window-buffer-names "*vc-diff*")
(add-to-list 'same-window-regexps ".*\\.c$")

(remove-hook 'find-file-hook
	     #'vc-find-file-hook)

;;-------------------------------------------------------------------------------
(require 'grep)

(add-to-list 'same-window-buffer-names "*grep*")

;;-------------------------------------------------------------------------------

(require 'server)
(setq server-log t)

(add-hook 'kill-emacs-hook #'basic-save-buffer)

;;-------------------------------------------------------------------------------

(load-ext "wm")
(load-ext "utils")
(load-ext "cc")
(load-ext "fonts")
(load-ext "env")

(load-ext "org" "org")
(load-ext "zencolor" "zenburn")
(load-ext "iresize" "iresize")
(load-ext "ac" "auto-complete")

(load-ext "wn" 'window-numbering-mode)
(load-ext "gtags" 'gtags-mode)
(load-ext "fs" 'flyspell-mode)
(load-ext "ispell" 'ispell-word)
(load-ext "psvn" 'svn-status)
(load-ext "root-win" 'split-root-window)

(unless (string= system-type "windows-nt")
  (load-ext "mpc" 'mpc)
  (load-ext "dictem" "dictem")
  (load-ext "nt" "newsticker")
  (load-ext "jabber" "emacs-jabber")
  (load-ext "wl" "wl")
  (load-ext "magit" "magit")
  (load-ext "w3m" "w3m")
  (load-ext "gdb"))

;;-------------------------------------------------------------------------------

(require 'buffer-recode)
(ring-insert evm-coding-systems-list 'windows-1251)
(ring-insert evm-coding-systems-list 'koi8-r)
(global-set-key [f5] 'recode-buffer)

;;-------------------------------------------------------------------------------
;; loaded finally to be sure that all mode maps are available

(load-ext "keys")

;;-------------------------------------------------------------------------------

(unless (string= system-type "windows-nt")
  (add-to-list 'initial-frame-alist '(name . "emacs"))
  (add-to-list 'default-frame-alist '(name . "emacs"))
  (add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))
  (when (functionp 'wl)
    (wl 1)))
