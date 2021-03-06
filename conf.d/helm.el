(require 'helm-config)
(require 'helm-buffers)
(helm-autoresize-mode 1)

(global-set-key (kbd "C-c h")   'helm-command-prefix)
(global-set-key (kbd "C-x b")   'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
(global-set-key (kbd "M-x")     'helm-M-x)
(global-set-key (kbd "M-y")     'helm-show-kill-ring)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(eval-after-load 'shell
  '(define-key shell-mode-map (kbd "C-c C-l") 'helm-comint-input-ring))
(define-key minibuffer-local-map (kbd "C-c C-l") 'helm-minibuffer-history)

(setf helm-split-window-in-side-p t       ; open helm buffer inside current window, not occupy whole other window
      helm-autoresize-max-height 20
      helm-move-to-line-cycle-in-source t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-file-name-history-use-recentf t
      helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match t
      helm-M-x-fuzzy-match t)

(dolist (entry ignored-buffer-list)
  (add-to-list 'helm-boring-buffer-regexp-list entry))

(add-to-list 'ignored-buffer-list "\\*helm")
