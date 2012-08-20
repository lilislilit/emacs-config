(setq term-scroll-to-bottom-on-output t)
(require 'term)
(setq dired-bind-jump nil)
(require 'dired-x)
(require 'sunrise-commander)

(setq sr-avfs-root "~/.avfs/mount")

(unless (string= system-type "windows-nt")
  (setq sr-attributes-display-mask '(nil nil nil nil t t t)))

(custom-set-faces
 '(sr-active-path-face ((t (:background "dark" :foreground "orange" :weight bold :height 160))))
 '(sr-passive-path-face ((t (:background "white" :foreground "grey" :weight bold :height 160)))))
