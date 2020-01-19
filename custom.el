(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(css-fontify-colors nil)
 '(custom-safe-themes
   (quote
    ("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default)))
 '(markdown-fontify-code-blocks-natively t)
 '(org-export-backends (quote (ascii html latex md hugo slimhtml)))
 '(package-selected-packages
   (quote
    (org ivy-rich org-bullets company auto-complete format-all undo-fu ox-latex winner-mode esup go-mode ox-tufte elixir-mode synosaurus ledger embrace avy edit-indirect ox-pandoc slim-mode ox-slimhtml handlebars-mode org-make-toc ox-hugo-auto-export emmet-mode shackle define-word yaml-mode which-key web-mode use-package spaceline smex simpleclip powerthesaurus ox-hugo multiple-cursors move-text markdown-mode magit haml-mode flx expand-region exec-path-from-shell cider)))
 '(pop-up-frames nil)
 '(safe-local-variable-values
   (quote
    ((eval add-hook
           (quote after-save-hook)
           (lambda nil
             (org-html-export-to-html t))
           t t)
     (org-babel-noweb-wrap-end . "»")
     (org-babel-noweb-wrap-start . "«")
     (org-confirm-babel-evaluate)
     (org-download-timestamp . "")
     (org-download-display-inline-images)
     (org-download-heading-lvl . 1)
     (org-download-image-dir . "../static/images/posts")
     (org-hugo-auto-export-on-save . t))))
 '(shell-pop-shell-type
   (quote
    ("ansi-term" "*ansi-term*"
     (lambda nil
       (ansi-term shell-pop-term-shell)))))
 '(shell-pop-universal-key nil)
 '(synosaurus-choose-method (quote default))
 '(vc-follow-symlinks t)
 '(word-wrap t)
 '(yascroll:delay-to-hide nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(line-number-current-line ((t (:inherit default :background "navajo white"))))
 '(markdown-code-face ((t nil)))
 '(markdown-inline-code-face ((t (:foreground "dark red"))))
 '(show-paren-match ((t (:background "grey84" :weight normal))))
 '(sp-pair-overlay-face ((t nil)))
 '(web-mode-current-element-highlight-face ((t (:background "gray88")))))
