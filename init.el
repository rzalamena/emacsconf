;;; init.el --- my personal configuration.

;;; Commentary:
;;
;; Package handling.
;;

;;; Code:
(package-initialize)

;; Repository list.
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

;; Packages to auto install.
(defvar package-list '(
                       ;; Auto complete.
                       company

                       ;; Languages support.
                       alchemist
                       clang-format
                       cmake-ide
                       cmake-mode
                       dockerfile-mode
                       gitignore-mode
                       gnuplot-mode
                       graphviz-dot-mode
                       markdown-mode
                       nginx-mode
                       vue-mode
                       web-mode
                       yaml-mode
                       yang-mode

                       ;; LSP plugins.
                       lsp-mode
                       lsp-ui

                       ;; Syntax checking.
                       flycheck
                       flycheck-clang-analyzer

                       ;; Project handling.
                       projectile

                       ;; Git handling.
                       magit

                       ;; UI improvements.
                       diff-hl
                       flx
                       flx-ido
                       helm
                       ido-vertical-mode
                       neotree
                       rainbow-delimiters
                       rainbow-mode
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

;; Load neotree and set the key binding.
(require 'neotree)
(setq neo-theme 'classic)
(global-set-key [f8] 'neotree-toggle)

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
(require 'lsp-mode)
(require 'lsp-ui)

;; Setup flycheck clang analyzer.
(require 'flycheck-clang-analyzer)
(flycheck-clang-analyzer-setup)

;; Disable LSP default flychecker.
(setq-default lsp-diagnostics-provider :none)

;; Set some key bindings.
(define-key lsp-mode-map (kbd "M-.") 'lsp-find-definition)
(define-key lsp-mode-map (kbd "M-]") 'lsp-find-references)

(global-company-mode t)
(setq company-idle-delay 0) ;; Faster auto completion.
(setq company-minimum-prefix-length 1) ;; Complete earlier.

;; Syntax checking.
(global-flycheck-mode t)

;; Project handling.
(require 'projectile)
(projectile-mode t)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; OpenBSD C style plus tweaks.
(require 'cc-mode)

(defconst openbsd-c-style
  '(
    ;; Use tabs and represent them with 8 spaces.
    (c-basic-offset . 8)
    (c-tab-always-indent . t)
    (indent-tabs-mode . t)

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
(add-hook 'c-mode-hook
          (lambda () (progn
                  (lsp-deferred)
                  (flycheck-select-checker 'c/c++-clang)
                  (c-set-style "openbsd")
                  )))

(add-hook 'cpp-mode-hook
          (lambda () (progn
                  (lsp-deferred)
                  (flycheck-select-checker 'c/c++-clang))))

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

;; Use two spaces indentation for shell scripting by default.
(setq-default sh-basic-offset 2)

;; Load languages support.
(require 'cmake-mode)
(require 'cmake-ide)
(cmake-ide-setup)

(require 'dockerfile-mode)
(require 'gnuplot-mode)
(require 'graphviz-dot-mode)
(require 'nginx-mode)
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
                           (lsp-deferred)
                           )))
(setq-default mmm-typescript-mode-enter-hook
              (lambda () (progn
                           (setq syntax-ppss-table nil)
                           (setq-default js-indent-level 2)
                           (lsp-deferred)
                           )))

;; According to ebuild developers manual we must use tabs instead of spaces.
;;
;; Source:
;; https://devmanual.gentoo.org/ebuild-writing/common-mistakes/index.html
;;
;; Last access: 2020-05-24.
;;
;; --- Quote begin ---
;; It is no fun reformatting lines of ebuilds because the submitter did not
;; follow the guidelines to use TABS rather than spaces. So please use tabs!
;; -- Quote end ---
(add-hook 'ebuild-mode-hook
          (lambda () (progn
                  (setq sh-basic-offset 8)
                  (setq indent-tabs-mode t))))

;; Load magit to get the key bindings.
(require 'magit)


;;
;; Emacs generated config:
;; load it from your personal file and don't clutter the versioned files.
;;
(setq custom-file "~/.emacs.d/custom.el")
(if (file-exists-p custom-file)
    (load custom-file))

;; Performance tweak: increase garbage colector threshold.
(setq gc-cons-threshold 50000000)

;; Performance tweak: increase read buffer size.
(setq read-process-output-max (* 1024 1024))

;; Silence a warning when editing Elixir projects:
;; node packages can easily reach 10k files in a project directory.
(setq lsp-file-watch-threshold 10000)

;; Start server by default.
(server-start)

(provide 'init)
;;; init.el ends here
