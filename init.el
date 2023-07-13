(defun gedeon/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                     (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'gedeon/display-startup-time)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/"))
(add-to-list 'package-archives '("elpa" . "https://elpa.gnu.org/packages/"))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(setq inhibit-startup-message t)

(scroll-bar-mode -1)  ; Disable visible scrollbar
(tool-bar-mode -1)    ; Disable the toolbar
(tooltip-mode -1)     ; Disable tooltips
(set-fringe-mode 0)  ; Give some breathing room

(menu-bar-mode -1)    ; Disable menu bar


;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)


;; SET THEME

(load-theme 'whiteboard)


;; LINE/COLUMN NUMBER S
(column-number-mode)
(global-display-line-numbers-mode t)


;; disable Line numbers for some modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		treemacs-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(use-package doom-themes
  :init (load-theme 'doom-tokyo-night t)
  (setq doom-themes-treemacs-theme "doom-colors")
  )

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 50)))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.2))

(use-package ivy
    :diminish
    :bind (("C-s" . swiper)
	   :map ivy-minibuffer-map
	   ("TAB" . ivy-alt-done)	
	   ("C-l" . ivy-alt-done)
	   ("C-j" . ivy-next-line)
	   ("C-k" . ivy-previous-line)
	   :map ivy-switch-buffer-map
	   ("C-k" . ivy-previous-line)
	   ("C-l" . ivy-done)
	   ("C-d" . ivy-switch-buffer-kill)
	   :map ivy-reverse-i-search-map
	   ("C-k" . ivy-previous-line)
	   ("C-d" . ivy-reverse-i-search-kill))
    :config
    (ivy-mode 1))

  
(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history)))

(require 'dashboard)
(dashboard-setup-startup-hook)
;; Or if you use use-package
(use-package dashboard
  :ensure t
  :config
  (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
  (setq dashboard-center-content t)
  (setq dashboard-icon-type 'all-the-icons)
  (setq dashoard-set-heading-icons t)
  (setq dashboard-set-navigator t)
  (dashboard-setup-startup-hook))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . helpful-function)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key))

(use-package hydra)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(use-package dirvish
  :config
  (dirvish-override-dired-mode)
  (setq dirvish-use-header-line 'global)    ; make header line span all panes

  (setq dirvish-header-line-height '(25 . 35))
  (setq dirvish-mode-line-height 25) ; shorthand for '(25 . 25)

  (setq dirvish-header-line-format
        '(:left (path) :right (free-space))
        dirvish-mode-line-format
        '(:left (sort file-time " " file-size symlink) :right (omit yank index)))
  (setq dirvish-attributes
        '(vc-state subtree-state all-the-icons collapse git-msg file-time file-size))
  :bind (
  :map dirvish-mode-map ; Dirvish inherits `dired-mode-map'
  ("a"   . dirvish-quick-access)
  ("q"   . dirvish-quit)
  ("f"   . dirvish-file-info-menu)
  ("y"   . dirvish-yank-menu)
  ("N"   . dirvish-narrow)
  ("^"   . dirvish-history-last)
  ("h"   . dirvish-history-jump) ; remapped `describe-mode'
  ("s"   . dirvish-quicksort)    ; remapped `dired-sort-toggle-or-edit'
  ("v"   . dirvish-vc-menu)      ; remapped `dired-view-file'
  ("TAB" . dirvish-subtree-toggle)
  ("M-f" . dirvish-history-go-forward)
  ("M-b" . dirvish-history-go-backward)
  ("M-l" . dirvish-ls-switches-menu)
  ("M-m" . dirvish-mark-menu)
  ("M-t" . dirvish-layout-toggle)
  ("M-s" . dirvish-setup-menu)
  ("M-e" . dirvish-emerge-menu)
  ("M-j" . dirvish-fd-jump)))

(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta)
  (setq mac-control-modifier 'control)
  (setq mac-option-modifier 'none))

(use-package general
  :config
  (general-create-definer gedeon/keys
    :keymaps '(normal insert visual emacs))
  (general-create-definer gedeon/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")
  (general-create-definer gedeon/local-leader-keys
  :keymaps '(normal insert visual emacs)
  :prefix "SPC m"
  :global-prefix "C-SPC m"))

(gedeon/leader-keys
  "t" '(:ignore t :which-key "toggles")
  "tt" '(counsel-load-theme :which-key "choose theme")
  "ts" '(hydra-text-scale/body :which-key "scale text")
  "tl" '(org-latex-preview :which-key "toggle latex preview"))

(gedeon/leader-keys
  "f" '(:ignore t :which-key "file")
  "ff" '(counsel-find-file :which-key "find")
  "fs" '(save-buffer :which-key "save file")
  "ft" '(treemacs :which-key "treemacs"))

(gedeon/leader-keys
  "b" '(:ignore t :which-key "buffer")
  "bb" '(counsel-switch-buffer :which-key "find"))

(gedeon/leader-keys
    "w" '(:ignore t :which-key "window")
    "wn" '(evil-window-vnew :which-key "new")
    "wl" '(evil-window-right :which-key "right")
    "wh" '(evil-window-left :which-key "left")
    "wj" '(evil-window-down :which-key "down")
    "wk" '(evil-window-up :which-key "up")
    "ww" '(evil-window-next :which-key "next")
    "wq" '(evil-window-delete :which-key "close"))

(gedeon/leader-keys
    "o" '(:ignore t :which-key "open")
    "oa" '(org-agenda :which-key "agenda"))

(gedeon/leader-keys
  "n" '(:ignore t :which-key "notes")
  "nf" '(org-roam-node-find :which-key "find")
  "nc" '(org-roam-capture :which-key "capture")
  "nb" '(org-roam-buffer-toggle :which-key "backlinks"))

(gedeon/leader-keys
  "p" '(:ignore t :which-key "project")
  "pp" '(counsel-projectile-switch-project :which-key "switch to project"))

(gedeon/leader-keys
  "h" '(:ignore t :which-key "help")
  "hv" '(counsel-describe-variable :which-key "describe variable")
  "hf" '(counsel-describe-function :which-key "describe function")
  "hb" '(describe-bindings :which-key "describe bindings"))

(gedeon/leader-keys
  "g" '(:ignore t :which-key "git")
  "gg" '(magit-status :which-key "magit status"))

(gedeon/leader-keys
  "c"  '(:ignore t :which-key "code")
  "cs" '(lsp-treemacs-symbols :which-key "scope tree")
  "cl" '(:ignore t :which-key "lisp")
  "cle" '(eval-buffer :which-key "eveluate lisp"))

(gedeon/leader-keys
  "." '(dirvish :which-key "dirvish"))

(defun gedeon/evil-hook ()
  (dolist (mode '(custom-mode
                  eshell-mode
                  git-rebase-mode
                  erc-mode
                  circe-server-mode
                  circe-chat-mode
                  circe-query-mode
                  sauron-mode
                  term-mode))
    (add-to-list 'evil-emacs-state-modes mode))
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))


(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)
  (define-key evil-motion-state-map (kbd "RET") nil)

  ;;use visuel line motions even outside of visual line mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))


(use-package evil-org
  :ensure t
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-escape
  :hook (evil-mode . evil-escape-mode)
  :config
  (setq evil-escape-key-sequence "jk"))

(evil-escape-mode 1)

(defun gedeon/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Open Sans" :weight 'semibold :height (cdr face)))


  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(defun gedeon/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1)
  (setq org-image-actual-width '(500 750 1000))
  (setq org-capture-templates `(
                                ("p" "Protocol" entry (file+headline ,(concat org-directory "notes.org") "Inbox")
                                 "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
                                ("L" "Protocol Link" entry (file+headline ,(concat org-directory "notes.org") "Inbox")
                                 "* %? [[%:link][%:description]] \nCaptured On: %U")
                                ))
  (gedeon/local-leader-keys
    "t" '(org-todo :which-key "todo state")
    "I" '(org-id-get-create :which-key "ad id")
    "a" '(org-attach :which-key "agenda")
    "e" '(org-export-dispatch :which-key "export")

    "b" '(:ignore t :which-key "babel")
    "be" '(org-babel-execute-src-block :which-key "execute")

    "n" '(:ignore t :which-key "node")
    "ni" '(org-roam-node-insert :which-key "insert link")
    "nf" '(org-roam-node-find :which-key "find node")
    "ns" '(org-narrow-to-subtree :which-key "narrow to subtree")
    "nw" '(widen :whichkey "widen")

    "s" '(:ignore t :which-key "search")
    "sr" '(org-recoll-search :which-key "recoll")
    "c" '(:ignore t :which-key "clock")
    "ci" '(org-clock-in :which-key "clock in")
    "co" '(org-clock-out :which-key "clock out")))

(use-package org
  :hook
  (org-mode . gedeon/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")
  (gedeon/org-font-setup)

  (setq org-agenda-files
        '("~/Dropbox/todo.org"))

  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-graph-column 60)

  (define-key org-agenda-mode-map "j" 'evil-next-line)
  (define-key org-agenda-mode-map "k" 'evil-previous-line)

  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
          (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))

  (setq org-refile-targets
        '(("archive.org" :maxlevel . 1)
          ("todo.org" :maxlevel . 1)))

  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  (setq org-tag-alist
        '((:startgroup)
                                        ; Put mutually exclusive tags here
          (:endgroup)
          ("@errand" . ?E)
          ("@work" . ?W)
          ("@home" . ?H)
          ("agenda" . ?a)
          ("planning" . ?p)
          ("publish" . ?P)
          ("batch" . ?b)
          ("note" . ?n)
          ("catchup" . ?c)
          ("idea" . ?i))))

(defun gedeon/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . gedeon/org-mode-visual-fill))

(use-package org-bullets
;;  :after org 
;;  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(require 'org-superstar)
(setq org-hide-leading-stars 100)
(add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))

(use-package org-fancy-priorities
:ensure t
:hook
(org-mode . org-fancy-priorities-mode)
:config
(setq org-fancy-priorities-list '("⚡" "⬆" "⬇" "☕")))

(use-package ob-rust)


(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (rust . t)
   (plantuml . t)
   (python . t)))

(setq org-confirm-babel-evaluate nil)

(defun gedeon/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
		      (expand-file-name "~/.config/emacs/emacs.org"))
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))
  (add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'gedeon/org-babel-tangle-config)))

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "~/org"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))

(load "~/.config/emacs/packages/org-recoll.el")
(global-set-key (kbd "C-c g") 'org-recoll-search)
(global-set-key (kbd "C-c u") 'org-recoll-update-index)

(require 'org-download)
(setq-default org-download-image-dir "~/org/images")
(add-hook 'dired-mode-hook 'org-download-enable)

(use-package lsp-mode
    :commands (lsp lsp-deferred)
    :bind (:map lsp-mode-map
                ("TAB" . completion-at-point))
    :hook (rust-mode . lsp-mode)
    :config
;;    (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
;;    (lsp-headerline-breadcrumb-mode)
    (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

(use-package company
:after lsp-mode
:hook (prog-mode . company-mode)
:bind (:map company-active-map
	    ("<tab>" . company-complete-selection))
(:map lsp-mode-map
      ("<tab>" . company-indent-or-complete-common))
:custom
(company-minimum-prefix-length 1)
(company-idle-delay 0.0))

(use-package company-box
  :hook (company-mode . company-box-mode))

(defun gedeon/rust-hook ()
    (setq indent-tabs-mode nil)
    (prettify-symbols-mode)
;;    (gedeon/local-leader-keys
;;    "r" '(rust-run :which-key "run")
;;     "c" '(rust-compile :which-key "compile"))
    )


  (require 'rust-mode)
  (add-hook 'rust-mode-hook
            (gedeon/rust-hook))
  (setq rust-format-on-save t)

(use-package flycheck-rust
:after (rust-mode)
:hook (flycheck-mode . flycheck-rust-setup))

(use-package ccls
:hook ((c-mode c++-mode objc-mode cuda-mode) .
       (lambda () (require 'ccls) (lsp)))
:config
(setq ccls-executable "/usr/bin/ccls"))

(use-package toml-mode)

(use-package rjsx-mode)

(use-package svelte-mode)

(use-package emmet-mode
:config
(gedeon/keys
 "C-e" '(emmet-expand-line :which-key "emmet expand")))

;;; Basic configuration
(require 'hledger-mode)
(add-hook 'hledger-mode-hook 'gedeon/hledger-hook)

(defun gedeon/hledger-hook ()
  (setq hledger-commodity-tab-alternative "EUR")
  (font-lock-add-keywords
   nil
   '(("~ monthly" . font-lock-constant-face))))


;; To open files with .journal extension in hledger-mode
(add-to-list 'auto-mode-alist '("\\.journal\\'" . hledger-mode))

;; Provide the path to you journal file.
;; The default location is too opinionated.
(setq hledger-jfile "~/Documents/finance/.hledger.journal")


;;; Auto-completion for account names
;; For company-mode users,
(add-to-list 'company-backends 'hledger-company)

(load "~/.config/emacs/packages/cook-mode.el")

(defun gedeon/cook-mode-setup ()
  (variable-pitch-mode 1))

(add-hook 'cook-mode-hook 'gedeon/cook-mode-setup)

(setq org-plantuml-jar-path (expand-file-name "~/.local/bin/plantuml.jar"))
(add-to-list 'org-src-lang-modes '("plantuml" . plantuml))

(require 'lsp-java)
(add-hook 'java-mode-hook #'lsp)

(use-package yasnippet
:config
(yas-global-mode 1))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/Projects/Code")
    (setq projectile-project-search-path '("~/Projects/Code")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

(use-package magit
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; evil-magit is now part of evil-collection

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))
