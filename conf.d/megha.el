(add-hook 'java-mode-hook
          (lambda ()
            (meghanada-mode t)
            (flycheck-mode +1)))

(let ((map (make-sparse-keymap)))
  (define-key map (kbd "M-d") #'meghanada-jump-declaration)
  (define-key map (kbd "M-r") #'meghanada-back-jump)
  (define-key meghanada-mode-map (kbd "M-j") map))

(cond
 ((eq system-type 'windows-nt)
  (setq meghanada-java-path (expand-file-name "bin/java.exe" (getenv "JAVA_HOME"))
        meghanada-maven-path "mvn.cmd"))
 (t
  (setq meghanada-java-path "java"
        meghanada-maven-path "mvn")))
