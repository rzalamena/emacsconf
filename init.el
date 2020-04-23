;;; init.el --- my personal configuration.

;;; Commentary:
;;
;; Package handling.
;;

;;; Code:
(package-initialize)

;; Repository list.
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;; Packages to auto install.
(defvar package-list '(
                       ;; Auto complete.
                       company

                       ;; Languages support.
                       alchemist
                       clang-format
                       dockerfile-mode
                       gitignore-mode
                       gnuplot-mode
                       graphviz-dot-mode
                       markdown-mode
                       vue-mode
                       web-mode
                       yaml-mode
                       yang-mode

                       ;; LSP plugins.
                       lsp-mode
                       lsp-ui
                       lsp-elixir
                       company-lsp
                       dap-mode

                       ;; Syntax checking.
                       flycheck
                       flycheck-clang-analyzer

                       ;; Project handling.
                       ggtags
                       projectile

                       ;; Git handling.
                       magit

                       ;; UI improvements.
                       all-the-icons
                       diff-hl
                       flx
                       flx-ido
                       helm
                       ido-vertical-mode
                       neotree
                       rainbow-delimiters
                       rainbow-mode

                       ;; Theme.
                       gruvbox-theme
                       ))

;; Detect non installed packages and install them.
(let (isrefreshed)
  (dolist (package package-list)
    (unless (package-installed-p package)
      ;; Check if we have ever refreshed the repositories, otherwise
      ;; do it to avoid error messages on boot-up.
      (unless isrefreshed
	(progn
	  (package-refresh-contents)
	  (setq isrefreshed t)))
      (package-install package))))


;;
;; GUI.
;;
(defalias 'yes-or-no-p 'y-or-n-p) ;; simplify yes/no with y/n.
(scroll-bar-mode -1) ;; remove scroll bar.
(menu-bar-mode -1) ;; remove menu bar.
(tool-bar-mode -1) ;; remove tool bar.
(setq inhibit-startup-screen t) ;; Don't show startup screen.
(column-number-mode t) ;; show column.

;; VCS gutter.
(require 'diff-hl)
(global-diff-hl-mode t)

;; Save settings and buffers.
(desktop-save-mode t)

;; Improved navigation.
(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(ido-vertical-mode t)
(flx-ido-mode 1)

(setq
 ido-enable-flex-matching t
 ;; disable ido faces to see flx highlights.
 ido-use-faces nil
 )

;; Enable helm.
(require 'helm-config)
(global-set-key (kbd "M-x") 'helm-M-x)

;; Make symbols pretty when possible.
(global-prettify-symbols-mode t)

;; Load icons for neotree and themes.
(require 'all-the-icons)
;; Note: you must run `all-the-icons-install-fonts' in order to get the
;; icons to show.

;; Load neotree and set the key binding.
(require 'neotree)
(setq neo-theme 'icons)
(global-set-key [f8] 'neotree-toggle)

;; Load theme.
(require 'gruvbox-theme)

;; Load rainbow & rainbow-delimiters mode.
(require 'rainbow-delimiters)
(require 'rainbow-mode)

(add-hook 'prog-mode-hook #'rainbow-mode)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)


;;
;; Editing.
;;
(electric-pair-mode t) ;; automatically create pairs, say parentheses.
(show-paren-mode t) ;; show matching parentheses.
(setq make-backup-files nil) ;; disable backup files.
(setq require-final-newline t) ;; add blank newline at the end files.
(global-auto-revert-mode t) ;; update buffer when disk file has changed.

;; Show annoying whitespaces.
(require 'whitespace)
(setq whitespace-style '(face trailing lines-tail empty
			      space-after-tab::tab space-after-tab::space
			      space-after-tab space-before-tab::tab
			      space-before-tab::space space-before-tab))
(global-whitespace-mode t)

;; Auto complete.
(require 'company)
(require 'company-lsp)
(require 'lsp-mode)
(require 'lsp-ui)
(require 'dap-mode)

;; Automatically configure lsp for all languages. Worst case is the
;; language is not supported and a warning message will be printed.
(add-hook 'prog-mode-hook #'lsp)

(global-company-mode t)
(setq company-idle-delay 0) ;; Faster auto completion.
(setq company-minimum-prefix-length 1) ;; Complete earlier.

;; Configure company to use the LSP backend.
(add-to-list 'company-backends '(company-lsp))

;; Syntax checking.
(global-flycheck-mode t)

;; Project handling.
(require 'projectile)
(projectile-mode t)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; OpenBSD C style plus tweaks.
(require 'cc-mode)

(defun c-newline ()
  "C newline implementation.

Check if we are inside a code comment to continue inside comment,
otherwise just use the regular newline function."
  (interactive)
  (let ((cont t) langelem)
    (save-excursion
      ;; Find the language element we are inside of.
      (while cont
	(setq langelem (caar (last (c-guess-basic-syntax))))
	(unless (eq langelem 'c)
	  (setq cont nil))
	(forward-line -1)))
    ;; If inside comment use comment newline otherwise use the default.
    (if (eq langelem 'comment-intro)
        (c-indent-new-comment-line)
      (newline-and-indent))))

(defconst openbsd-c-style
  '(
    ;; Use tabs and represent them with 8 spaces.
    (c-basic-offset . 8)
    (c-tab-always-indent . t)

    ;; Labels start at column 0 of the indentation.
    (c-label-minimum-indentation . 0)

    ;; When commenting with 'M-;' use the style(9) format.
    (comment-style . extra-line)

    ;; Some items to make code editing more pleasant.
    (add-to-list 'c-cleanup-list
		 '(brace-else-brace
		   brace-elseif-brace
		   defun-close-semi
		   compact-empty-funcall)))
  "OpenBSD C style.")
(c-add-style "openbsd" openbsd-c-style)

;; Automatically configure C buffers to use my preference.
(add-hook 'c-mode-hook (lambda () (progn
			       (ggtags-mode t)
			       (c-set-style "openbsd")
			       (define-key c-mode-map (kbd "RET") 'c-newline))))

;; Setup flycheck clang analyzer.
(require 'flycheck-clang-analyzer)
(flycheck-clang-analyzer-setup)

;; HTML editing note:
;; elixir auto completion inside HTML is broken, alchemist doesn't like
;; completing without `elixir-mode' (neither does lsp).
;;
;; Elixir editing note:
;; Requering alchemist automatically configures elixir buffers to use it.
(require 'alchemist)
;; Call `M-x customize', find `alchemist-goto-elixir-source-dir' and
;; `alchemist-goto-erlang-source-dir' and set them in order to be able
;; to jump to implementations of Erlang and Elixir functions.
;;
;; You can also edit `custom.el' yourself and specify the values there.
;;
;; 2020-04-23:
;; To go to source code definitions (Erlang or Elixir) with lsp you must
;; call `alchemist-goto-definition-at-point' manually with `M-x'.

;; With the exception of C, all other languages expect spaces instead of
;; tabs.
(setq-default indent-tabs-mode nil)

;; Always indent by default.
(define-key global-map (kbd "RET") 'newline-and-indent)

;; Load languages support.
(require 'dockerfile-mode)
(require 'gnuplot-mode)
(require 'graphviz-dot-mode)
(require 'vue-mode)
(require 'yaml-mode)
(require 'yang-mode)

;; Fix vue javascript/typescript indentation.
;;
;; Read issue for more information:
;; https://github.com/AdamNiederer/vue-mode/issues/100
(setq-default mmm-js-mode-enter-hook
              (lambda () (progn
                      (setq syntax-ppss-table nil)
                      (setq-default js-indent-level 2)
                      (lsp)
                      )))
(setq-default mmm-typescript-mode-enter-hook
              (lambda () (progn
                      (setq syntax-ppss-table nil)
                      (setq-default js-indent-level 2)
                      (lsp)
                      )))

;; Load magit to get the key bindings.
(require 'magit)


;;
;; Emacs generated config:
;; load it from your personal file and don't clutter the versioned files.
;;
(setq custom-file "~/.emacs.d/custom.el")
(if (file-exists-p custom-file)
    (load custom-file))

(provide 'init)
;;; init.el ends here
