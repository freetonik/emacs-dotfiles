(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(css-fontify-colors nil)
 '(custom-safe-themes
   (quote
    ("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default)))
 '(markdown-fontify-code-blocks-natively nil)
 '(org-agenda-files
   (quote
    ("~/Google Drive/Knowledgebase/org/journal.org" "~/.emacs.d/init.org" "/Users/rakhim/Google Drive/Knowledgebase/org/archive.org" "/Users/rakhim/Google Drive/Knowledgebase/org/links.org" "/Users/rakhim/Google Drive/Knowledgebase/org/main.org")))
 '(org-export-backends (quote (ascii html latex md hugo slimhtml)))
 '(package-selected-packages
   (quote
    (ox-tufte smartparens writegood-mode elixir-mode synosaurus ledger-mode ledger embrace change-inner avy edit-indirect ox-pandoc slim-mode clj-refactor ox-slimhtml handlebars-mode org-make-toc ox-hugo-auto-export emmet-mode shackle neotree define-word yaml-mode which-key web-mode visual-regexp use-package spaceline smex simpleclip shell-pop powerthesaurus ox-hugo multiple-cursors move-text markdown-mode magit haml-mode flx expand-region exec-path-from-shell dumb-jump company cider)))
 '(pop-up-frames nil)
 '(safe-local-variable-values
   (quote
    ((org-babel-noweb-wrap-end . "»")
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
 '(shell-pop-universal-key "s-=")
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
