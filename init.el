(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

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
;; (set-fringe-mode 10)  ; Give some breathing room

(menu-bar-mode -1)    ; Disable menu bar


;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)


;; SET THEME

(load-theme 'whiteboard)


;; LINE/COLUMN NUMBERS
(column-number-mode)
(global-display-line-numbers-mode t)


;; disable Line numbers for some modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; (if (eq system-type 'darwin)
;;  (set-face-attribute 'default nil :font "JetBrains Mono" :height 150)
;;  (set-face-attribute 'fixed-pitch nil :font "JetBrains Mono" :height 150)
;;  (set-face-attribute 'variable-pitch nil :font "OpenSans" :height 150)
;;  (setq mac-command-modifier 'meta)
;;  (setq mac-control-modifier 'control)
;;  (setq mac-option-modifier 'super)
;; )

(if (eq system-type 'gnu/linux)
  (set-face-attribute 'default nil :font "JetBrainsMonoNL Nerd Font" :height 150)
  (set-face-attribute 'fixed-pitch nil :font "JetBrainsMonoNL Nerd Font" :height 150)
  (set-face-attribute 'variable-pitch nil :font "OpenSans" :height 150)
)

(use-package doom-themes
  :init (load-theme 'doom-tokyo-night t))

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
 "ts" '(hydra-text-scale/body :which-key "scale text"))

(gedeon/leader-keys
 "f" '(:ignore t :which-key "file")
 "ff" '(counsel-find-file :which-key "find")
 "fs" '(save-buffer :which-key "save file"))

(gedeon/leader-keys
 "b" '(:ignore t :which-key "buffer")
 "bb" '(counsel-switch-buffer :which-key "find"))


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
    (add-to-list 'evil-emacs-state-modes mode)))


(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :hook (evil-mode . gedeon/evil-hook)
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
    (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face)))


  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(use-package org
  :hook (org-mode . gedeon/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")
  (gedeon/org-font-setup)

  (setq org-agenda-files
	'("~/org/todo.org")
	'("~/org/habits.org"))

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
		   "| %U | %^{Weight} | %^{Notes} |" :kill-buffer t)))



	  )

(use-package org-bullets
  :after org 
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun gedeon/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . gedeon/org-mode-visual-fill))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)))

(setq org-confirm-babel-evaluate nil)

(defun gedeon/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
		      (expand-file-name "~/.config/emacs/emacs.org"))
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))
  (add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'gedeon/org-babel-tangle-config)))

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

(gedeon/leader-keys
  "fp" '(counsel-projectile-switch-project :which-key "switch project"))

(use-package magit
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; evil-magit is now part of evil-collection

(gedeon/leader-keys
  "g" '(:ignore t :which-key "git"))

(gedeon/leader-keys
  "gg" '(magit-status :which-key "magit status"))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))
