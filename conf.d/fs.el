(define-key flyspell-mode-map (kbd "C-c i") #'ispell-change-dictionary)

(add-hook 'text-mode-hook #'(lambda () (flyspell-mode 1)))
(add-hook 'org-mode-hook #'(lambda () (flyspell-mode 1)))
