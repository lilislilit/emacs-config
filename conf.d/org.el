(require 'org-agenda)

(setq
 calendar-week-start-day 1
 org-catch-invisible-edits 'show-and-error
 org-time-stamp-custom-formats '("<%a %d %b %Y>" . "<%a %H:%M %d %b %Y>")
 org-agenda-window-setup 'current-window)

(define-key org-mode-map (kbd "C-c C-x C-a") nil)
(define-key org-agenda-mode-map (kbd "C-c C-x C-a") nil)

(add-hook 'org-mode-hook #'org-indent-mode)
(add-hook 'org-mode-hook #'org-toggle-time-stamp-overlays)

(add-to-list 'auto-mode-alist '("\\.org\\(-mode\\)?$" . org-mode))

(defadvice org-agenda-redo-all
    (around reload-files (command &optional exhaustive)
            activate)
  "it reloads files from disk before rebuilding agenda"
  (dolist (file org-agenda-files)
    (find-file-noselect file)))
