(let ((map (define-prefix-command 'my-counsel-gtags-prefix-mode-map)))
  (define-key map (kbd "M-f") #'counsel-gtags-find-file)
  (define-key map (kbd "M-s") #'counsel-gtags-find-symbol)
  (define-key map (kbd "M-r") #'counsel-gtags-find-reference)
  (define-key map (kbd "M-d") #'counsel-gtags-find-definition)
  (define-key map (kbd "M-t") #'counsel-gtags-dwim)
  (define-key counsel-gtags-mode-map (kbd "M-j") map))

(define-key counsel-gtags-mode-map (kbd "M-,") 'counsel-gtags-pop)

