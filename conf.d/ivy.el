(setq ivy-use-virtual-buffers t
      ivy-count-format "[%d/%d] ")

(dolist (buf ignored-buffer-list)
  (add-to-list 'ivy-ignore-buffers buf))

(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
