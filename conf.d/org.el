(require 'org-agenda)

(setq
 calendar-week-start-day 1
 org-agenda-window-setup 'current-window)

(define-key org-mode-map (kbd "C-c C-x C-a") nil)
(define-key org-agenda-mode-map (kbd "C-c C-x C-a") nil)

(add-hook 'org-mode-hook #'org-indent-mode)

(add-to-list 'auto-mode-alist '("\\.org\\(-mode\\)?$" . org-mode))
