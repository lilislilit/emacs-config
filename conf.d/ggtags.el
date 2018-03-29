(define-key ggtags-mode-prefix-map (kbd "M-t") #'ggtags-find-tag-dwim)
(define-key ggtags-mode-prefix-map (kbd "M-r") #'ggtags-find-reference)

(add-hook 'c-mode-common-hook #'ggtags-mode)
