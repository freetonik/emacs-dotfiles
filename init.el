;;; -*- lexical-binding: t -*-
(setq gc-cons-threshold (* 50 1000 1000))

;; Startup timer
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; -----------
;; USE PACKAGE

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                 (not (gnutls-available-p))))
    (proto (if no-ssl "http" "https")))
    (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
    (when (< emacs-major-version 24)
      (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile (require 'use-package))
(setq use-package-always-ensure t)

;; this package is useful for overriding major mode keybindings
(use-package bind-key)

;; Emacs control is Ctrl. Emacs Super is Command. Emacs Meta is Alt. Right Alt (option) can be used to enter symbols like em dashes =—=.
(setq
 mac-right-command-modifier 'super
 mac-command-modifier 'super
 mac-option-modifier 'meta
 mac-left-option-modifier 'meta
 mac-right-option-modifier 'meta
 mac-right-option-modifier 'nil)

(global-set-key (kbd "s-=") 'text-scale-increase)
(global-set-key (kbd "s--") 'text-scale-decrease)

;; -------
;; VISUALS

(load-theme 'tsdh-light)
(setq-default line-spacing 0)
;; (set-face-attribute 'default nil :font "Inconsolata LGC 15")
(set-face-attribute 'default nil :font "SF Mono 15")
;; (show-paren-mode)
;; (set-face-attribute 'show-paren-match nil :weight 'extra-bold)
;; (set-face-background 'show-paren-match "wheat")
(tool-bar-mode -1)
;; run =defaults write org.gnu.Emacs TransparentTitleBar LIGHT= when using macport emacs
(scroll-bar-mode -1)
(blink-cursor-mode 0)
(global-visual-line-mode t)
;; Show full path in the title bar.
(setq-default frame-title-format "%b (%f)")

;; yes...
(use-package nyan-mode)

;; Show columns in addition to rows in mode line
(setq column-number-mode t)

;; Never use tabs, use spaces instead.
(setq-default
 indent-tabs-mode nil
 c-basic-indent 2
 c-basic-offset 2
 tab-width 2)

(setq
 tab-width 2
 js-indent-level 2
 css-indent-offset 2
 c-basic-offset 2)

;; I don't care about auto save and backup files.
(setq
 make-backup-files nil  ; stop creating backup~ files
 auto-save-default nil  ; stop creating #autosave# files
 create-lockfiles nil)  ; stop creating .# files

;; Revert (update) buffers automatically when underlying files are changed externally.
(global-auto-revert-mode t)

;; Some basic things.
(setq
 inhibit-startup-message t         ; Don't show the startup message
 inhibit-startup-screen t          ; or screen
 cursor-in-non-selected-windows t  ; Hide the cursor in inactive windows

 echo-keystrokes 0.1               ; Show keystrokes right away, don't show the message in the scratch buffer
 initial-scratch-message nil       ; Empty scratch buffer
 sentence-end-double-space nil     ; Sentences should end in one space, come on!
 confirm-kill-emacs 'y-or-n-p      ; y and n instead of yes and no when quitting
)

(fset 'yes-or-no-p 'y-or-n-p)      ; y and n instead of yes and no everywhere else
(delete-selection-mode 1)
(global-unset-key (kbd "s-p"))

;; I want Emacs kill ring and system clipboard to be independent. Simpleclip is the solution to that.

(use-package simpleclip
  :init
  (simpleclip-mode 1))

;; Eval expr regardless of mode
(global-set-key (kbd "C-s-<return>") (kbd "C-x C-e"))

;; Quickly switch to scratch buffer with =Cmd+0=:
(global-set-key (kbd "s-0") (lambda ()
                              (interactive)
                              (if (string= (buffer-name) "*scratch*")
                                  (previous-buffer)
                                (switch-to-buffer "*scratch*"))))

;; This is great for learning Emacs, it shows a nice table of possible commands.
(use-package which-key
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1.5))

;; --------------
;; OS integration

;; Pass system shell environment to Emacs. This is important primarily for shell inside Emacs, but also things like Org mode export to Tex PDF don't work, since it relies on running external command =pdflatex=, which is loaded from =PATH=.
(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

;; Use =Cmd+i= to open the current folder in a new tab of iTerm:
(defun iterm-goto-filedir-or-home ()
  "Go to present working dir and focus iterm"
  (interactive)
  (do-applescript
   (concat
    " tell application \"iTerm2\"\n"
    "   tell current window\n"
    "     create tab with profile \"Default\"\n"
    "   end tell\n"
    "   tell the current session of current window\n"
    (format "     write text \"cd %s\" \n"
            ;; string escaping madness for applescript
            (replace-regexp-in-string "\\\\" "\\\\\\\\"
                                      (shell-quote-argument (or default-directory "~"))))
    "   end tell\n"
    " end tell\n"
    " do shell script \"open -a iTerm\"\n"
    ))
  )
(global-set-key (kbd "s-i") 'iterm-goto-filedir-or-home)

;; ----------------------
;; Navigation and editing

;; Kill line with =s-Backspace=, which is =Cmd-Backspace=. Note that thanks to Simpleclip, killing doesn't rewrite the system clipboard. Kill one word by =Alt-Backspace=. Also, kill forward word with =Alt-Shift-Backspace=, since =Alt-Backspace= is kill word backwards.
(global-set-key (kbd "s-<backspace>") 'kill-whole-line)
(global-set-key (kbd "s-<delete>") 'kill-whole-line)
(global-set-key (kbd "M-S-<backspace>") 'kill-word)
(global-set-key (kbd "M-<delete>") 'kill-word)
(bind-key* "S-<delete>" 'kill-word)

;; Use =super= (which is =Cmd=) for movement and selection just like in macOS.
(global-set-key (kbd "s-<right>") 'end-of-visual-line)
(global-set-key (kbd "s-<left>") 'beginning-of-visual-line)
(global-set-key (kbd "s-<up>") 'beginning-of-buffer)
(global-set-key (kbd "s-<down>") 'end-of-buffer)
(global-set-key (kbd "s-l") 'goto-line)

;; Basic things you should expect from macOS.
(global-set-key (kbd "s-a") 'mark-whole-buffer)       ;; select all
(global-set-key (kbd "s-s") 'save-buffer)             ;; save
(global-set-key (kbd "s-S") 'write-file)              ;; save as
(global-set-key (kbd "s-q") 'save-buffers-kill-emacs) ;; quit

;; Regular undo-redo.
(use-package undo-fu)
(global-unset-key (kbd "C-z"))
(global-set-key (kbd "C-z")   'undo-fu-only-undo)
(global-set-key (kbd "C-S-z") 'undo-fu-only-redo)
(global-set-key (kbd "s-z")   'undo-fu-only-undo)
(global-set-key (kbd "s-r")   'undo-fu-only-redo)

;; Avy for fast navigation.
(use-package avy
  :defer t
  :config
  (global-set-key (kbd "s-;") 'avy-goto-char-timer))

;; Auto completion
(use-package company
  :bind (("<backtab>" . 'company-complete))
  :config
  (global-company-mode))

;; alternative
;; (use-package auto-complete
;;   :config
;;   (ac-config-default))

(use-package smartparens
  :config
  ;; (require 'smartparens-config)
  (smartparens-global-mode t)
  (show-smartparens-global-mode t)
  (setq sp-show-pair-delay 0)
  ;; no '' pair in emacs-lisp-mode
  (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
  (sp-local-pair 'markdown-mode "`"   nil :actions '(wrap insert))  ;; only use ` for wrap and auto insertion in markdown-mode
  (sp-local-tag 'markdown-mode "s" "```scheme" "```")
  (define-key smartparens-mode-map (kbd "C-s-<right>") 'sp-forward-slurp-sexp)
  (define-key smartparens-mode-map (kbd "C-s-<left>") 'sp-forward-barf-sexp))

;; Go back to previous mark (position) within buffer and go back (forward?).
(defun my-pop-local-mark-ring ()
  (interactive)
  (set-mark-command t))

(defun unpop-to-mark-command ()
  "Unpop off mark ring. Does nothing if mark ring is empty."
  (interactive)
      (when mark-ring
        (setq mark-ring (cons (copy-marker (mark-marker)) mark-ring))
        (set-marker (mark-marker) (car (last mark-ring)) (current-buffer))
        (when (null (mark t)) (ding))
        (setq mark-ring (nbutlast mark-ring))
        (goto-char (marker-position (car (last mark-ring))))))

(global-set-key (kbd "s-,") 'my-pop-local-mark-ring)
(global-set-key (kbd "s-.") 'unpop-to-mark-command)

;; Since =Cmd+,= and =Cmd+.= move you back in forward in the current buffer, the same keys with =Shift= move you back and forward between open buffers.
(global-set-key (kbd "s-<") 'previous-buffer)
(global-set-key (kbd "s->") 'next-buffer)

;; Go to other windows easily with one keystroke =s-something= instead of =C-x something=.
(defun vsplit-last-buffer ()
  (interactive)
  (split-window-vertically)
  (other-window 1 nil)
  (switch-to-next-buffer))

(defun hsplit-last-buffer ()
  (interactive)
  (split-window-horizontally)
  (other-window 1 nil)
  (switch-to-next-buffer))

(global-set-key (kbd "s-w") (kbd "C-x 0")) ;; just like close tab in a web browser
(global-set-key (kbd "s-W") (kbd "C-x 1")) ;; close others with shift
(global-set-key (kbd "s-T") 'vsplit-last-buffer)
(global-set-key (kbd "s-t") 'hsplit-last-buffer)
(global-set-key (kbd "C-s-k") 'kill-this-buffer)

;; Expand-region allows to gradually expand selection inside words, sentences, etc. =C-'= is bound to Org's =cycle through agenda files=, which I don't really use, so I unbind it here before assigning global shortcut for expansion.
(use-package expand-region
  :config
  (global-set-key (kbd "s-'") 'er/expand-region)
  (global-set-key (kbd "s-\"") 'er/contract-region))

;; =Move-text= allows moving lines around with meta-up/down.
(use-package move-text
  :config
  (move-text-default-bindings))

(use-package crux
  :bind (("C-a" . crux-move-beginning-of-line)
         ("s-<return>" . crux-smart-open-line)
         ("s-S-<return>" . crux-smart-open-line-above)
         ("s-R" . crux-rename-file-and-buffer)))

;; Join lines whether you're in a region or not.
(defun smart-join-line (beg end)
  "If in a region, join all the lines in it. If not, join the current line with the next line."
  (interactive "r")
  (if mark-active
      (join-region beg end)
      (top-join-line)))

(defun top-join-line ()
  "Join the current line with the next line."
  (interactive)
  (delete-indentation 1))

(defun join-region (beg end)
  "Join all the lines in the region."
  (interactive "r")
  (if mark-active
      (let ((beg (region-beginning))
            (end (copy-marker (region-end))))
        (goto-char beg)
        (while (< (point) end)
          (join-line 1)))))

(global-set-key (kbd "s-j") 'smart-join-line)

;; Upcase word and region using the same keys.
(global-set-key (kbd "M-u") 'upcase-dwim)
(global-set-key (kbd "M-l") 'downcase-dwim)

;; Delete trailing spaces and add new line in the end of a file on save.
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq require-final-newline t)

;; Multiple cusors are a must. Make <return> insert a newline; multiple-cursors-mode can still be disabled with C-g.
(use-package multiple-cursors
  :config
  (setq mc/always-run-for-all 1)
  (global-set-key (kbd "s-d") 'mc/mark-next-like-this)
  (global-set-key (kbd "s-D") 'mc/mark-all-dwim)
  (define-key mc/keymap (kbd "<return>") nil))

;; Comment lines.
(global-set-key (kbd "s-/") 'comment-line)

;; -------
;; WINDOWS

;; Automatic windows are always created on the bottom, not on the side.
(setq
 split-height-threshold 0
 split-width-threshold nil)

;; Move between windows with Control-Command-Arrow and with =Cmd= just like in iTerm.
(global-set-key (kbd "s-o") (kbd "C-x o"))

(use-package windmove
  :config
  (global-set-key (kbd "s-[")  'windmove-left)         ;; Cmd+[ go to left window
  (global-set-key (kbd "s-]")  'windmove-right)        ;; Cmd+] go to right window
  (global-set-key (kbd "s-{")  'windmove-up)           ;; Cmd+Shift+ go to upper window
  (global-set-key (kbd "<s-}>")  'windmove-down))      ;; Ctrl+Shift+ go to down window

;; Enable winner mode to quickly restore window configurations
(winner-mode 1)

;; Shackle to make sure all windows are nicely positioned.
(use-package shackle
  :init
  (setq shackle-default-alignment 'below
        shackle-default-size 0.4
        shackle-rules '((help-mode           :align below :select t)
                        (helpful-mode        :align below)
                        (compilation-mode    :select t   :size 0.25)
                        ("*compilation*"     :select nil :size 0.25)
                        ("*ag search*"       :select nil :size 0.25)
                        ("*Flycheck errors*" :select nil :size 0.25)
                        ("*Warnings*"        :select nil :size 0.25)
                        ("*Error*"           :select nil :size 0.25)
                        ("*Org Links*"       :select nil :size 0.1)
                        (magit-status-mode                :align bottom :size 0.5  :inhibit-window-quit t)
                        (magit-log-mode                   :same t                  :inhibit-window-quit t)
                        (magit-commit-mode                :ignore t)
                        (magit-diff-mode     :select nil  :align left   :size 0.5)
                        (git-commit-mode                  :same t)
                        (vc-annotate-mode                 :same t)
                        ))
  :config
  (shackle-mode 1))

;; Edit indirect
;; Select any region and edit it in another buffer.
;; TODO: if region selected, invoke edit indirect with same keystroke as narrow
(use-package edit-indirect)

;; -----------------------
;; IVY, SWIPER AND COUNSEL
(use-package ivy
  :config
  (ivy-mode 1)
  (setq
   ivy-use-virtual-buffers t
   ivy-count-format "(%d/%d) "
   enable-recursive-minibuffers t
   ivy-initial-inputs-alist nil
   ivy-re-builders-alist
   '((swiper . ivy--regex-plus)
     (swiper-isearch . regexp-quote)
     ;; (counsel-git . ivy--regex-plus)
     ;; (counsel-ag . ivy--regex-plus)
     (counsel-rg . ivy--regex-plus)
     (t      . ivy--regex-fuzzy)))   ;; enable fuzzy searching everywhere except for Swiper and ag

  (global-set-key (kbd "s-b") 'ivy-switch-buffer))

(use-package swiper
  :config
  (global-set-key (kbd "s-f") 'swiper-isearch))

(use-package counsel
  :config
  ;; When using git ls (via counsel-git), include unstaged files
  (setq counsel-git-cmd "git ls-files -z --full-name --exclude-standard --others --cached --")
  (setq ivy-initial-inputs-alist nil)

  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "s-y") 'counsel-yank-pop)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "s-F") 'counsel-rg)
  (global-set-key (kbd "s-p") 'counsel-git))

(use-package smex)
(use-package flx)

(use-package ivy-rich
  :config
  (ivy-rich-mode 1)
  (setq ivy-rich-path-style 'abbrev)) ;; To abbreviate paths using abbreviate-file-name (e.g. replace “/home/username” with “~”

;; -----
;; MAGIT

;; Navigate to projects with =Cmd+Shift+P= (thanks to reddit user and emacscast listener fritzgrabo):
(defun magit-status-with-prefix-arg ()
  "Call `magit-status` with a prefix."
  (interactive)
  (let ((current-prefix-arg '(4)))
    (call-interactively #'magit-status)))

(use-package magit
  :config
  (setq magit-repository-directories '(("\~/code" . 4) ("\~/Dropbox/projects/" . 4)))
  (global-set-key (kbd "s-P") 'magit-status-with-prefix-arg)
  (global-set-key (kbd "s-g") 'magit-status))

;; --------------
;; SPELL CHECKING
;; Spell checking requires an external command to be available. Install =aspell= on your Mac, then make it the default checker for Emacs' =ispell=. Note that personal dictionary is located at =~/.aspell.LANG.pws= by default.
(setq ispell-program-name "aspell")

(use-package flyspell-popup
  :config
  (global-set-key (kbd "s-\\") 'flyspell-popup-correct))

;; Enable spellcheck on the fly for all text modes. This includes org, latex and LaTeX. Spellcheck current word.
(add-hook 'text-mode-hook 'flyspell-mode)
;; (global-set-key (kbd "s-\\") 'ispell-word)
(global-set-key (kbd "C-s-\\") 'flyspell-auto-correct-word)

;; ---------
;; THESAURUS

;; Spellcheck was =Cmd+\=, synonym search is =Cmd+Shift+\=. It requires =wordnet= to be installed locally.
;; (use-package powerthesaurus
;;   :config
;;   (global-set-key (kbd "s-|") 'powerthesaurus-lookup-word-dwim)
;;   )

;; Alternative, local thesaurus
(use-package synosaurus
  :config
  (global-set-key (kbd "s-|") 'synosaurus-choose-and-replace))

;; Word definition search.
(use-package define-word
  :config
  (global-set-key (kbd "M-\\") 'define-word-at-point))

;; ------
;; ABBREV
(read-abbrev-file abbrev-file-name t)
(setq-default abbrev-mode t)

;; ---------
;; YASNIPPET

;; YASnippet is a template system for Emacs. It allows you to type an abbreviation and automatically expand it into function templates.
(use-package yasnippet
  :defer t
  :config
  (setq yas-snippet-dirs
        '("~/.emacs.d/snippets"))
  (yas-global-mode 1))

;; ----
;; HUGO

(use-package easy-hugo
  :init
  (setq easy-hugo-basedir "~/code/rakhim.org/")
  (setq easy-hugo-url "https://rakhim.org")
  (setq easy-hugo-postdir "content/blog")
  (setq easy-hugo-server-flags "-D")
  (setq easy-hugo-image-directory "images/posts")
  (setq easy-hugo-previewtime "300")
  :bind ("C-c C-e" . easy-hugo))

;; (load "~/.emacs.d/elisp/markdown-dnd-images.el")
(use-package markdown-dnd-images
  :load-path "~/.emacs.d/elisp/"
  ;; :config
  ;; (setq dnd-save-directory "../../static/images/posts/")
  )

;; --------
;; MARKDOWN

;; Utility functions for export.
(defun markdown-export-html-to-clipboard (lines-to-skip)
  "Export Markdown to HTML while skipping first lines-to-skip, copy to cliboard"
  (interactive)
  (markdown-kill-ring-save)
  (let ((oldbuf (current-buffer)))
    (save-current-buffer
      (set-buffer "*markdown-output*")
      (if lines-to-skip
          (progn
            (goto-char (point-min))
            (kill-whole-line)))
      (with-no-warnings (mark-whole-buffer))
      (simpleclip-copy (point-min) (point-max)))))

(defun markdown-export-html-to-clipboard-full ()
  (interactive) (markdown-export-html-to-clipboard nil))

(defun markdown-export-html-to-clipboard-no-1st-line ()
  (interactive) (markdown-export-html-to-clipboard 1))

;; =Cmd-O= to copy Markdown as raw HTML to clipboard. Widen and narrow like in Org.

(use-package markdown-mode
  :defer t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :bind (:map markdown-mode-map
              ("s-k" . 'markdown-insert-link)
              ("C-s-<down>" . 'markdown-narrow-to-subtree)
              ("C-s-<up>" . 'widen)
              ("s-O" . 'markdown-export-html-to-clipboard-full)
              ("M-s-O" . 'markdown-export-html-to-clipboard-no-1st-line))
  :init (setq markdown-command '("pandoc" "--no-highlight")))

;; -----------
;; PROGRAMMING

(use-package yaml-mode
  :defer t
  :mode (("\\.yml\\'" . yaml-mode)
         ("\\.yaml\\'" . yaml-mode)))

(use-package clojure-mode
  :defer t)
(use-package cider
  :defer t)

(use-package web-mode
  :defer t
  :mode ("\\.html\\'")
  :config
  (setq web-mode-markup-indent-offset 2))

(use-package emmet-mode
  :defer t
  :init
  (setq
   emmet-indentation 2
  emmet-move-cursor-between-quotes t)
  :config
  (add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
  (add-hook 'web-mode-hook  'emmet-mode)
  (add-hook 'html-mode-hook 'emmet-mode)
  (add-hook 'css-mode-hook  'emmet-mode)) ;; enable Emmet's css abbreviation.

(use-package format-all)

;; ------------------------
;; FRAMES, WINDOWS, BUFFERS

;; Always open files in the same frame, even when double-clicked from Finder.
(setq ns-pop-up-frames nil)

(defun kill-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

;; --------
;; ORG MODE

;; slimhtml is an emacs org mode export backend. It is a set of transcoders for common org elements which outputs minimal HTML.
(use-package htmlize)
(use-package ox-slimhtml)

(defun org-html-export-as-slimhtml
  (&optional async subtreep visible-only body-only ext-plist)
  (interactive)
  (org-export-to-buffer 'slimhtml "*slimhtml*"
    async subtreep visible-only body-only ext-plist (lambda () (html-mode))))

;; Utility functions for export.

(defun my-org-export-html-to-clipboard (skipline)
  (interactive)
  (org-html-export-as-html nil nil t t)
  (if skipline
      (progn
        (goto-char (point-min))
        (kill-whole-line)
        (kill-whole-line)
        (goto-char (point-max))
        (forward-line -1)
        (kill-whole-line)))
  (with-no-warnings (mark-whole-buffer))
  (simpleclip-copy (point-min) (point-max))
  (delete-window))

(defun my-org-export-html-to-clipboard-full () (interactive) (my-org-export-html-to-clipboard nil))
(defun my-org-export-html-to-clipboard-no-1st-line () (interactive)  (my-org-export-html-to-clipboard 1))

(use-package org
  :config
  (setq
   org-startup-indented t
   org-catch-invisible-edits 'error
   org-cycle-separator-lines -1
   calendar-week-start-day 1
   org-ellipsis "⤵"
   org-directory "/Users/rakhim/Dropbox/Org"
   org-agenda-files '("/Users/rakhim/Dropbox/Org")
   org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9)))
   org-src-tab-acts-natively t
   org-src-preserve-indentation t
   org-src-fontify-natively t

   ;; Enable speed keys to manage headings without arrows.
   org-use-speed-commands t

   ;; Allow shift selection with arrows.
   ;; This will not interfere with some built-in shift+arrow functionality.
   org-support-shift-select t)

  :config
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (global-set-key (kbd "\e\ec") (lambda () (interactive) (find-file "~/.emacs.d/init.el")))

  :bind (:map org-mode-map
              ;; I want to retain Shift-Alt movement and selection everywhere, but in Org mode these bindings are important built ins, and I don't know if there is a viable alternative.
              ("<S-left>" . nil)
              ("<S-right>" . nil)
              ("<M-left>" . nil)
              ("<M-right>" . nil)
              ;; no shift-alt with arrows
              ("<M-S-left>" . nil)
              ("<M-S-right>" . nil)

              ("C-s-<left>" . 'org-metaleft)
              ("C-s-<right>" . 'org-metaright)
              ("C-'" . nil)
              ("C-s-<down>" . 'org-narrow-to-subtree)
              ("C-s-<up>" . 'widen)
              ("M-s-o" . 'my-org-export-html-to-clipboard-no-1st-line)
              ("s-O" . 'my-org-export-html-to-clipboard-full)))

;; --------------
;; Small things

;; Store custom-file separately, don't freak out when it's not found.
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; Start an Emacs server. This way, I can use emacsclient from the terminal.
(unless (server-running-p) (server-start))

;; To enable easy emacsclient, create =~/bin/ec=:
;; #!/bin/sh
;; # this assumes that my regular emacs app runs the server as part of startup
;; emacsclient -n "$@" || (open -a emacs "$@")

(use-package unkillable-scratch
  :ensure t
  :config (unkillable-scratch t))

;; THE END
;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))
