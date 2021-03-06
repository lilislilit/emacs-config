(require 'cl)
(require 'warnings)

(menu-bar-mode 0)
(tool-bar-mode 0)
(toggle-scroll-bar -1)
(column-number-mode 1)

(transient-mark-mode 1)
(delete-selection-mode 1)

(electric-pair-mode 1)

(setq
 inhibit-startup-screen t
 visible-bell t
 make-pointer-invisible t
 x-select-enable-clipboard t
 default-input-method 'russian-computer
 browse-url-browser-function 'browse-url-firefox)

(put 'scroll-left 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(add-to-list 'warning-suppress-types '(undo discard-info))

(add-to-list 'same-window-buffer-names "*grep*")

(add-hook 'kill-emacs-hook #'basic-save-buffer)

(setenv "GPG_AGENT_INFO" nil)

(defvar ignored-buffer-list
  '("\\*Completions" "\\*Quail Completions\\*" "\\*Backtrace\\*" "\\*magit-edit-log\\*" "\\*P4" "\\*Buffer List\\*")
  "list of the buffer names or regular expressions to be ignored by
various buffer management routines")

;;-------------------------------------------------------------------------------

(defun suitable-buffer-p (buffer)
  "predicate to check the buffer exclusion from the `ignored-buffer-list'"
  (not (find-if #'(lambda (entry)
                    (string-match entry (buffer-name buffer)))
                ignored-buffer-list)))

(setf frame-title-format "%F")
(add-to-list 'default-frame-alist `(buffer-predicate . ,#'suitable-buffer-p))
(add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))

;;-------------------------------------------------------------------------------

(add-to-list 'auto-mode-alist '("Makefile\\..*" . makefile-mode))
(add-to-list 'auto-mode-alist '(".*\\.mak$" . makefile-mode))
(add-to-list 'auto-mode-alist '(".*\\.h\\.in$" . c-mode))

;;-------------------------------------------------------------------------------

(global-visual-line-mode 1)
(setq visual-line-fringe-indicators '(nil right-curly-arrow))

;;-------------------------------------------------------------------------------

(add-hook 'text-mode-hook
          #'(lambda()
              (define-key text-mode-map (kbd "M-q") #'unfill-paragraph)))

;;-------------------------------------------------------------------------------
;; e/common lisp

(defun lisp-no-tabs()
  "it disables tabs indentation"
  (setq indent-tabs-mode nil))

(add-hook 'emacs-lisp-mode-hook #'lisp-no-tabs)
(add-hook 'lisp-mode-hook #'lisp-no-tabs)

;;-------------------------------------------------------------------------------

(global-whitespace-mode 1)
(setq whitespace-style '(face
                         trailing
                         space-after-tab
                         space-before-tab))

;;-------------------------------------------------------------------------------
;; diff

(add-hook 'diff-mode-hook
          '(lambda ()
             ;; diff-goto-source
             (define-key diff-mode-map (kbd "C-m") 'diff-goto-source)))

;;-------------------------------------------------------------------------------

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward
      uniquify-ignore-buffers-re "^\\*")

;;-------------------------------------------------------------------------------
;; package management

(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "http://stable.melpa.org/packages/")
   t)
  (package-initialize))

(let ((my-load-path (expand-file-name "~/.emacs.d/loadable/")))
  (add-to-list 'load-path my-load-path)
  (dolist (entry (directory-files my-load-path t nil t))
    (when (and (file-directory-p entry)
               (equal 'nil (string-match "/\\.\\.?$" entry)))
      (add-to-list 'load-path (expand-file-name entry)))))

;;-------------------------------------------------------------------------------

(require 'local-env nil t)
(require 'my-utils)

;; third party modules
(load-package 'iresize
              :after-load ((global-set-key (kbd "C-c r") #'iresize-mode))
              :required t)

(load-package 'window-numbering
              :after-load ((window-numbering-mode 1))
              :required t)

(load-package 'expand-region
              :after-load ((global-set-key (kbd "M-@") 'er/expand-region))
              :required t)

(load-package 'which-key
              :options ((which-key-idle-delay . 2.0))
              :after-load ((which-key-mode 1))
              :required t)

(load-package 'aggressive-indent
              :after-load ((global-aggressive-indent-mode 1))
              :required t)

(load-package 'company
              :after-load ((global-company-mode 1)
                           (global-set-key (kbd "M-t") #'company-complete))
              :required t)

(load-package 'ggtags
              :options ((ggtags-mode-prefix-key . (kbd "M-j"))
                        (ggtags-enable-navigation-keys . nil)
                        (ggtags-split-window-function . #'split-window-vertically))
              :config "ggtags")

(load-package 'helm
              :options ((helm-command-prefix-key . (kbd "C-x C-h")))
              :config "helm")
(load-package 'helm-gtags :config "helm-gtags")

(load-package 'ivy :config "ivy")
(load-package 'counsel-gtags :config "counsel-gtags")

(load-package 'powerline
              :after-load ((powerline-default-theme))
              :required t)

;; embedded packages
(load-package 'ispell :config "spell")
(load-package 'flyspell :config "fs" :required t)
(load-package 'org :config "org")
(load-package 'dired :config "dired")
(load-package 'cc-mode :config "cc")
(load-package 'erc :config "erc")
(load-package 'ido
              :options ((ido-enable-flex-matching . t)
                        (ido-create-new-buffer . 'always))
              :after-load ((dolist (entry ignored-buffer-list)
                             (add-to-list 'ido-ignore-buffers entry))))

;;-------------------------------------------------------------------------------
(global-set-key (kbd "C-x l")
                (lexical-let (swap-last)
                  #'(lambda ()
                      "that's a wrapper around the `swap-buffers'
function to keep a state variable"
                      (interactive)
                      (swap-buffers swap-last)
                      (setq swap-last (not swap-last)))))
(global-set-key (kbd "C-x c") #'shell-jump)
(global-set-key (kbd "C-x M-f") #'find-file-at-point)
(global-set-key (kbd "C-x C-x") #'server-edit)

;; general commands start from C-c
(global-set-key (kbd "C-c g") #'rgrep)
(global-set-key (kbd "C-c w") #'browse-url)
;; window management in StumpWM style :)
(global-set-key (kbd "C-c s") #'split-window-horizontally)
(global-set-key (kbd "C-c v") #'split-window-vertically)
(global-set-key (kbd "C-c q") #'delete-other-windows)
(global-set-key (kbd "C-c k") #'delete-window)
(global-set-key (kbd "C-c o") #'other-window)

(global-set-key (kbd "C-c C-x C-a") #'org-agenda)
(global-set-key (kbd "C-c C-x g") #'magit-status)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file :noerror)
