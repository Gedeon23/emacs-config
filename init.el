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

(use-package general
  :config
  (general-create-definer gedeon/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC"))

(gedeon/leader-keys
  "t" '(:ignore t :which-key "toggles")
  "tt" '(counsel-load-theme :which-key "choose theme")
  "ts" '(hydra-text-scale/body :which-key "scale text")
  "tl" '(org-latex-preview :which-key "toggle latex preview"))

(gedeon/leader-keys
  "f" '(:ignore t :which-key "file")
  "ff" '(counsel-find-file :which-key "find")
  "fs" '(save-buffer :which-key "save file")
  "ft" '(treemacs :which-key "treemacs")
  "fp" '(counsel-projectile-switch-project :which-key "switch project"))

(gedeon/leader-keys
  "b" '(:ignore t :which-key "buffer")
  "bb" '(counsel-switch-buffer :which-key "find"))

(gedeon/leader-keys
  "h" '(:ignore t :which-key "help")
  "hv" '(counsel-describe-variable :which-key "describe variable")
  "hf" '(counsel-describe-function :which-key "describe function")
  "hb" '(describe-bindings :which-key "describe bindings"))

(gedeon/leader-keys
  "g" '(:ignore t :which-key "git")
  "gg" '(magit-status-here :which-key "magit status"))

(gedeon/leader-keys
  "c"  '(:ignore t :which-key "code")
  "cs" '(lsp-treemacs-symbols :which-key "scope tree")
  "cl" '(:ignore t :which-key "lisp")
  "cle" '(eval-buffer :which-key "eveluate lisp")
  "cb" '(:ignore t :which-key "code block")
  "cbe" '(org-babel-execute-src-block :which-key "execute"))

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

  ;;use visuel line motions even outside of visual line mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-escape
  :hook (evil-mode . evil-escape-mode)
  :config
  (setq evil-escape-key-sequence "jk"))

(evil-escape-mode 1)

(defun gedeon/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1)
  (gedeon/leader-keys
    "m" '(:ignore t :which-key "org-mode")
    "mt" '(org-todo :which-key "todo state")
    "mI" '(org-id-get-create :which-key "ad id")
    "mn" '(:ignore t :which-key "node")
    "mni" '(org-roam-node-insert :which-key "insert link")))


(use-package org
  :hook
  (org-mode . gedeon/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")
  (gedeon/org-font-setup)

  (setq org-agenda-files
        '("~/org/todo.org"
          "~/org/habits.org"))

  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-graph-column 60)

  (define-key org-agenda-mode-map "j" 'evil-next-line)
  (define-key org-agenda-mode-map "k" 'evil-previous-line)

  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
          (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))

  (setq org-refile-targets
        '(("Archive.org" :maxlevel . 1)
          ("Tasks.org" :maxlevel . 1)))

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
          ("idea" . ?i)))

  (setq org-agenda-custom-commands
        '(("d" "Dashboard"
           ((agenda "" ((org-deadline-warning-days 7)))
            (todo "NEXT"
                  ((org-agenda-overriding-header "Next Tasks")))
            (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))


          (setq org-capture-templates
                `(("t" "Tasks / Projects")
                  ("tt" "Task" entry (file+olp "~/org/todo.org" "Inbox")
                   "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)

                  ("j" "Journal Entries")
                  ("jj" "Journal" entry
                   (file+olp+datetree "~/org/journal.org")
                   "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
                   ;; ,(dw/read-file-as-string "~/Notes/Templates/Daily.org")
                   :clock-in :clock-resume
                   :empty-lines 1)
                  ("jm" "Meeting" entry
                   (file+olp+datetree "~/Projects/Code/emacs-from-scratch/OrgFiles/Journal.org")
                   "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
                   :clock-in :clock-resume
                   :empty-lines 1)

                  ("w" "Workflows")
                  ("we" "Checking Email" entry (file+olp+datetree "~/Projects/Code/emacs-from-scratch/OrgFiles/Journal.org")
                   "* Checking Email :email:\n\n%?" :clock-in :clock-resume :empty-lines 1)

                  ("m" "Metrics Capture")
                  ("mw" "Weight" table-line (file+headline "~/Projects/Code/emacs-from-scratch/OrgFiles/Metrics.org" "Weight")
                   "| %U | %^{Weight} | %^{Notes} |" :kill-buffer t))))))

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
    (set-face-attribute (car face) nil :font "OpenSans" :weight 'semibold :height (cdr face)))


  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

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

(require 'org-download)

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

(use-package lsp-treemacs
:after lsp)

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
  (gedeon/leader-keys
   "m" '(:ignore t :which-key "rust-mode")
   "mr" '(rust-run :which-key "run")
   "mc" '(rust-compile :which-key "compile"))
  )


(require 'rust-mode)
(add-hook 'rust-mode-hook
          (gedeon/rust-hook))
(setq rust-format-on-save t)

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

(global-why-this-mode)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(flycheck-swiftlint yasnippet why-this which-key visual-fill-column use-package typescript-mode rust-mode rainbow-delimiters org-superstar org-roam-ui org-noter-pdftools org-fancy-priorities org-download org-bullets ob-rust magit lsp-ui lsp-treemacs ivy-rich helpful general flycheck-eglot evil-escape evil-dvorak evil-collection doom-themes doom-modeline counsel-projectile company-box command-log-mode all-the-icons aas)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
