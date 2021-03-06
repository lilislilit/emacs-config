(setq ivy-use-virtual-buffers t
      ivy-count-format "[%d/%d] ")

(dolist (buf ignored-buffer-list)
  (add-to-list 'ivy-ignore-buffers buf))

(global-set-key (kbd "C-c C-s") 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)

(define-key ivy-minibuffer-map (kbd "M-t") #'ivy-partial-or-done)
