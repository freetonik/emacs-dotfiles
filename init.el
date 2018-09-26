(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                 (not (gnutls-available-p))))
    (proto (if no-ssl "http" "https")))
    ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
    (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
    ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
    (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
(add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile (require 'use-package))

(setq use-package-always-ensure t)

(setq mac-right-command-modifier 'super)
(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'super)

(setq mac-right-option-modifier 'nil)

;;; -*- lexical-binding: t -*-

(defun tangle-init ()
  "If the current buffer is 'init.org' the code-blocks are
tangled, and the tangled file is compiled."
  (when (equal (buffer-file-name)
               (expand-file-name (concat user-emacs-directory "init.org")))
    ;; Avoid running hooks when tangling.
    (let ((prog-mode-hook nil))
      (org-babel-tangle)
      (byte-compile-file (concat user-emacs-directory "init.el")))))

(add-hook 'after-save-hook 'tangle-init)

(eval-when-compile
  (setq use-package-expand-minimally byte-compile-current-file))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

(when (memq window-system '(mac ns))
  (add-to-list 'default-frame-alist '(ns-appearance . light)) ;; {light, dark}
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t)))

(load-theme 'tsdh-light)

;; (use-package rich-minority
;;   :config
;;   (rich-minority-mode 1)
;;   (setf rm-blacklist ""))

(set-face-attribute 'default nil :font "Inconsolata 18")
(setq-default line-spacing 1)
(setq initial-frame-alist '((top . 10) (left . 10) (width . 125) (height . 45)))
(tool-bar-mode -1)

;; (require 'paren)
;; (setq show-paren-delay 0)
;; (show-paren-mode 1)
(set-face-background 'show-paren-match "PeachPuff2")
(set-face-foreground 'show-paren-match "maroon")
(set-face-attribute 'show-paren-match nil :weight 'extra-bold)

(use-package smartparens
  :config
  (require 'smartparens-config)
  (smartparens-global-mode t)
  (show-smartparens-global-mode t)
  (setq sp-show-pair-delay 0)

  ;; no '' pair in emacs-lisp-mode
  (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
  (sp-local-pair 'markdown-mode "`"   nil :actions '(wrap insert))  ;; only use ` for wrap and auto insertion in markdown-mode
)

(global-visual-line-mode 1)

(setq column-number-mode t) ;; show columns in addition to rows in mode line
(set-face-attribute 'mode-line nil :background "NavajoWhite")
(set-face-attribute 'mode-line-inactive nil :background "grey93")

(use-package vi-tilde-fringe
  :config
  (global-vi-tilde-fringe-mode 1))

(setq-default frame-title-format "%b (%f)")

(setq-default indent-tabs-mode nil)
(setq tab-width 2)

(setq js-indent-level 2)
(setq css-indent-offset 2)
(setq-default c-basic-offset 2)
(setq c-basic-offset 2)
(setq-default tab-width 2)
(setq-default c-basic-indent 2)

(use-package which-key
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.5))

(blink-cursor-mode 0)

(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files
(setq create-lockfiles nil)  ; stop creating .# files

(global-auto-revert-mode t)

(setq
 inhibit-startup-message t         ; Don't show the startup message
 inhibit-startup-screen t          ; or screen
 cursor-in-non-selected-windows t  ; Hide the cursor in inactive windows

 echo-keystrokes 0.1               ; Show keystrokes right away, don't show the message in the scratch buffe
 initial-scratch-message nil       ; Empty scratch buffer
 initial-major-mode 'org-mode      ; org mode by default
 sentence-end-double-space nil     ; Sentences should end in one space, come on!
 confirm-kill-emacs 'y-or-n-p      ; y and n instead of yes and no when quitting
 ;; help-window-select t              ; select help window so it's easy to quit it with 'q'
)

(fset 'yes-or-no-p 'y-or-n-p)      ; y and n instead of yes and no everywhere else
(scroll-bar-mode -1)
(delete-selection-mode 1)
(global-unset-key (kbd "s-p"))
(global-hl-line-mode nil)

(use-package simpleclip
  :config
  (simpleclip-mode 1))

(global-set-key (kbd "s-<backspace>") 'kill-whole-line)
(global-set-key (kbd "M-S-<backspace>") 'kill-word)

(global-set-key (kbd "s-<right>") (kbd "C-e"))
(global-set-key (kbd "S-s-<right>") (kbd "C-S-e"))
(global-set-key (kbd "s-<left>") (kbd "M-m"))
(global-set-key (kbd "S-s-<left>") (kbd "M-S-m"))

(global-set-key (kbd "s-<up>") 'beginning-of-buffer)
(global-set-key (kbd "s-<down>") 'end-of-buffer)

(global-set-key (kbd "s-a") 'mark-whole-buffer)       ;; select all
(global-set-key (kbd "s-s") 'save-buffer)             ;; save
(global-set-key (kbd "s-S") 'write-file)              ;; save as
(global-set-key (kbd "s-q") 'save-buffers-kill-emacs) ;; quit

;; (global-set-key (kbd "s-z") 'undo)

(use-package undo-tree
  :init
  (progn
    (global-undo-tree-mode)
    (setq undo-tree-auto-save-history nil)
    (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/tmp/undo"))
          undo-tree-auto-save-history nil)))

(global-set-key (kbd "s-z") 'undo-tree-undo)
(global-set-key (kbd "s-Z") 'undo-tree-redo)

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

(global-set-key (kbd "s-<") 'previous-buffer)
(global-set-key (kbd "s->") 'next-buffer)

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

(global-set-key (kbd "s-o") (kbd "C-x o"))

(global-set-key (kbd "s-w") (kbd "C-x 0")) ;; just like close tab in a web browser
(global-set-key (kbd "s-W") (kbd "C-x 1")) ;; close others with shift

(global-set-key (kbd "s-T") 'vsplit-last-buffer)
(global-set-key (kbd "s-t") 'hsplit-last-buffer)

(use-package expand-region
  :config
  (global-set-key (kbd "s-'") 'er/expand-region))

(use-package move-text
  :config
  (move-text-default-bindings))

(defun smart-open-line ()
  "Insert an empty line after the current line. Position the cursor at its beginning, according to the current mode."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

(defun smart-open-line-above ()
  "Insert an empty line above the current line. Position the cursor at it's beginning, according to the current mode."
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))

(global-set-key (kbd "s-<return>") 'smart-open-line)
(global-set-key (kbd "s-S-<return>") 'smart-open-line-above)

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
;; (global-set-key (kbd "s-J") 'smart-join-line)

;; (global-set-key (kbd "s-i") 'previous-line)
;; (global-set-key (kbd "s-k") 'next-line)
;; (global-set-key (kbd "s-j") 'left-char)
;; (global-set-key (kbd "s-l") 'right-char)

(global-set-key (kbd "M-u") 'upcase-dwim)
(global-set-key (kbd "M-l") 'downcase-dwim)

(use-package visual-regexp
  :config
  (define-key global-map (kbd "s-r") 'vr/replace))

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq require-final-newline t)

(use-package multiple-cursors
  :config
  (setq mc/always-run-for-all 1)
  (global-set-key (kbd "s-d") 'mc/mark-next-like-this)
  (global-set-key (kbd "M-s-d") 'mc/edit-beginnings-of-lines)
  (global-set-key (kbd "s-D") 'mc/mark-all-dwim)
  (define-key mc/keymap (kbd "<return>") nil))

(global-set-key (kbd "s-/") 'comment-line)

(put 'dired-find-alternate-file 'disabled nil)

(use-package dired
  :ensure nil
  :custom
  (dired-auto-revert-buffer t)
  (dired-dwim-target t)
  (dired-hide-details-hide-symlink-targets nil)
  (dired-listing-switches "-alh")
  (dired-ls-F-marks-symlinks nil)
  (dired-recursive-copies 'always))

;; (setq split-height-threshold 0)
;; (setq split-width-threshold nil)

(use-package windmove
  :config
  (global-set-key (kbd "s-[")  'windmove-left)         ;; Cmd+[ go to left window
  (global-set-key (kbd "s-]")  'windmove-right)        ;; Cmd+] go to right window
  (global-set-key (kbd "s-{")  'windmove-up)           ;; Cmd+Shift+[ go to upper window
  (global-set-key (kbd "<s-}>")  'windmove-down))      ;; Ctrl+Shift+[ go to down window

(winner-mode 1)
(global-set-key (kbd "C-s-[") 'winner-undo)
(global-set-key (kbd "C-s-]") 'winner-redo)

(use-package shackle
  :init
  (setq shackle-default-alignment 'below
        shackle-default-size 0.4
        shackle-rules '((help-mode           :align below :select t)
                        (helpful-mode        :align below)
                        (dired-mode          :ignore t)

                        (compilation-mode    :select t   :size 0.25)
                        ("*compilation*"     :select nil :size 0.25)
                        ("*ag search*"       :select nil :size 0.25)
                        ("*Flycheck errors*" :select nil :size 0.25)
                        ("*Warnings*"        :select nil :size 0.25)
                        ("*Error*"           :select nil :size 0.25)

                       http://localhost:1313/1/01/for-google-you-re-neither-a-user-nor-a-product-dot-you-re-data-point-dot/images/google_inbox.jpg ("*Org Links*"       :select nil   :size 0.2)

                        (" *undo-tree*"                   :align right  :size 0.3)
                        (neotree-mode                     :align left)
                        (magit-status-mode                :align bottom :size 0.5  :inhibit-window-quit t)
                        (magit-log-mode                   :same t                  :inhibit-window-quit t)
                        (magit-commit-mode                :ignore t)
                        (magit-diff-mode     :select nil  :align left   :size 0.5)
                        (git-commit-mode                  :same t)
                        (vc-annotate-mode                 :same t)
                        ("^\\*git-gutter.+\\*$" :regexp t :size 15 :noselect t)
                        ))
  :config
  (shackle-mode 1))
  ;; (defun my/shackle-defaults (plist)
  ;;   "Ensure popups are always aligned and selected by default. Eliminates the need
  ;;  for :align t on every rule."
  ;;   (when plist
  ;;     (unless (or (plist-member plist :align)
  ;;                 (plist-member plist :same)
  ;;                 (plist-member plist :frame))
  ;;       (plist-put plist :align t))
  ;;     (unless (or (plist-member plist :select)
  ;;                 (plist-member plist :noselect))
  ;;       (plist-put plist :select t)))
  ;;   plist)
  ;; (advice-add #'shackle--match :filter-return #'my/shackle-defaults)

  ;; (add-hook 'my/after-init-hook 'shackle-mode))

(setq scroll-margin 10
   scroll-step 1
   next-line-add-newlines nil
   scroll-conservatively 10000
   scroll-preserve-screen-position 1)

(setq mouse-wheel-follow-mouse 't)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))

(use-package projectile
  :config
  (define-key projectile-mode-map (kbd "s-P") 'projectile-command-map)
  (projectile-mode +1))

(use-package ivy
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq enable-recursive-minibuffers t)
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-re-builders-alist
      '((swiper . ivy--regex-plus)
        (t      . ivy--regex-fuzzy)))   ;; enable fuzzy searching everywhere except for Swiper

  (global-set-key (kbd "s-b") 'ivy-switch-buffer)
  (global-set-key (kbd "M-s-b") 'ivy-resume))

(use-package swiper
  :config
  (global-set-key "\C-s" 'swiper)
  (global-set-key "\C-r" 'swiper)
  (global-set-key (kbd "s-f") 'swiper))

(use-package counsel
  :config
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file))

(use-package smex)
(use-package flx)
(use-package avy)

(use-package ivy-rich
  :config
  (ivy-rich-mode 1)
  (setq ivy-rich-path-style 'abbrev)) ;; To abbreviate paths using abbreviate-file-name (e.g. replace “/home/username” with “~”

(use-package counsel-projectile
  :config
  (counsel-projectile-mode 1)
  (global-set-key (kbd "s-F") 'counsel-projectile-ag)
  (global-set-key (kbd "s-p") 'counsel-projectile))

(setq projectile-completion-system 'ivy)

(use-package magit
  :config
  (global-set-key (kbd "s-g") 'magit-status))

(use-package git-gutter
  :config
  (global-git-gutter-mode 't)
  (set-face-background 'git-gutter:modified 'nil) ;; background color
  (set-face-foreground 'git-gutter:added "green4")
  (set-face-foreground 'git-gutter:deleted "red"))

(use-package exec-path-from-shell)

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(use-package shell-pop)

(setq ispell-program-name "aspell")

(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

(global-set-key (kbd "s-\\") 'ispell-word)

(use-package powerthesaurus
  :config
  (global-set-key (kbd "s-|") 'powerthesaurus-lookup-word-dwim))

(use-package define-word
  :config
  (global-set-key (kbd "M-\\") 'define-word-at-point))

(use-package company
  :config
  (setq company-idle-delay 0.1)
  (setq company-global-modes '(not org-mode markdown-mode))
  (setq company-minimum-prefix-length 1)
  (add-hook 'after-init-hook 'global-company-mode))

(use-package yaml-mode)
(use-package markdown-mode)
(use-package haml-mode)
(use-package clojure-mode)
(use-package cider)

(use-package web-mode
  :mode ("\\.html\\'")
  :config
  (setq web-mode-markup-indent-offset 2))

;; (set-frame-name "EDIT")
;; (make-frame '((name . "ORG")))

;; (progn
;; (make-frame '((name . "TERM")))
;;   (select-frame-by-name "EDIT")
;;   (multi-term))
;; (make-frame '((name . "ORG")))

;; (global-set-key (kbd "s-1") (lambda () (interactive) (select-frame-by-name "EDIT")))
;; (global-set-key (kbd "s-2") (lambda () (interactive) (select-frame-by-name "TERM")))
;; (global-set-key (kbd "s-3") (lambda () (interactive) (select-frame-by-name "ORG")))

(use-package org
  :config
  (setq org-startup-indented t))

(setq org-directory "~/org")

(setq org-agenda-files '("~/org"))

(setq org-support-shift-select t)

(eval-after-load 'org
  '(progn
    (add-to-list 'org-structure-template-alist '("el" "#+BEGIN_SRC emacs-lisp \n?\n#+END_SRC"))
    (define-key org-mode-map (kbd "C-'") nil)
    (global-set-key "\C-ca" 'org-agenda)))

(setq org-src-tab-acts-natively t)
(setq org-src-preserve-indentation t)
(setq org-src-fontify-natively t)

;; (find-file "~/org/main.org")

(setq org-log-into-drawer t)

(defun org-mode-export-links ()
  "Export links document to HTML automatically when 'links.org' is changed"
  (when (equal (buffer-file-name) "/Users/rakhim/org/links.org")
    (progn
      (org-html-export-to-html)
      (message "HTML exported"))))

(add-hook 'after-save-hook 'org-mode-export-links)

(global-set-key (kbd "\e\em") (lambda () (interactive) (find-file "~/org/main.org")))
(global-set-key (kbd "\e\ec") (lambda () (interactive) (find-file "~/.emacs.d/init.org")))
(global-set-key (kbd "\e\el") (lambda () (interactive) (find-file "~/org/links.org")))
(global-set-key (kbd "\e\eb") (lambda () (interactive) (find-file "~/org/rakhim.org")))

(global-set-key (kbd "C-c c") 'org-capture)

(setq org-cycle-separator-lines 1)

(setq org-log-done 'time)

(use-package ox-hugo
  :after ox)

(require 'ox-hugo-auto-export)

;; Populates only the EXPORT_FILE_NAME property in the inserted headline.
(require 'org)
(require 'ox-hugo)
;; define variable to get rid of 'reference to free variable' warnings
(defvar org-capture-templates nil)
(with-eval-after-load 'org-capture
  (defun org-hugo-new-subtree-post-capture-template ()
    "Returns `org-capture' template string for new Hugo post.
See `org-capture-templates' for more information."
    (let* ((title (read-from-minibuffer "Post Title: ")) ;Prompt to enter the post title
           (fname (org-hugo-slug title)))
      (mapconcat #'identity
                 `(
                   ,(concat "* TODO " title)
                   ":PROPERTIES:"
                   ,(concat ":EXPORT_FILE_NAME: " fname)
                   ":END:"
                   "%?\n")          ;Place the cursor here finally
                 "\n")))

  (add-to-list 'org-capture-templates
               '("h"
                 "Hugo post at rakhim.org"
                 entry
                 (file+olp "rakhim.org" "Blog")
                 (function org-hugo-new-subtree-post-capture-template))))
