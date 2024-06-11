;; Place all local configuration options here

(if (equal window-system 'w32)

    ;; All the following code will be executed ONLY if window-system is 'w32
    (progn
      (setenv "HOME" "C:\\Users\\recordr\\AppData\\Roaming\\")
      (setenv "PATH" (concat (getenv "PATH") ":C:/ProgramData/chocoportable/bin"))
      (setq exec-path (append exec-path '("C:/ProgramData/chocoportable/bin")))

      ;; The rest are wrapped into progn to allow after-load to function
      (progn
	(with-eval-after-load "ispell"
          (add-to-list 'ispell-hunspell-dict-paths-alist '("en_US" "C:/Hunspell/en_US.aff")))
	(global-unset-key (kbd "<scroll>"))
	(global-set-key (kbd "<scroll>") 'ignore)
	(setq w32-scroll-lock-modifier nil)
	(setq ispell-program-name "hunspell")
	(setenv "LANG" "en_US")
	(setq ispell-dictionary "en_US")
	)

      ) ;; End of the progn

  ;; Code for when window-system is 'x (or anything other than 'w32)
  ;; Add what should be executed only when under X windows
  (if (equal window-system 'x)
      (progn
	;; Your X11-specific configurations here
	))
  ) ;; End of the main if


(use-package emacs
  :init
  (set-language-environment "UTF-8")
  (set-default-coding-systems 'utf-8-unix)
  (setq confirm-kill-processes nil)		; Stop confirming the killing of processes
  (setq use-short-answers t)                      ; y-or-n-p makes answering questions faster
  (setq read-process-output-max (* 1024 1024))    ; Increase the amount of data which Emacs reads from the process
  (setq gc-cons-threshold 100000000)
  (setq lsp-idle-delay 0.500)
  )

;; M-x customize-group RET notmuch RET
;; (autoload 'notmuch "notmuch" "notmuch mail" t)

(setq sentence-end-double-space nil)

;; In an emacs lisp scratch buffer you can run this code to double
;; check value of variable:
;; (message "Current fill-column value: %d" fill-column)
;; M-x display-fill-column-indicator-mode
;; to show vertical line at the fill-column
;; highlight text and press ALT-q to fill text to that width
(setq-default fill-column 80)

;; Default to y/n instead of yes/no
(defalias 'yes-or-no-p 'y-or-n-p)

(setq visible-bell t)

(blink-cursor-mode 0)

(recentf-mode 1)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(toggle-frame-maximized)

(when (equal window-system 'x)
  (setq python-shell-interpreter "/usr/bin/python3.12"))

;; Automatically create closing parenthesis/quote
(electric-pair-mode 1)

(use-package org
  )

;; Enable Visual Line Mode to wrap at fill column
(use-package visual-fill-column
  :config
  (add-hook 'visual-line-mode-hook #'visual-fill-column-mode))

;;; For packaged versions which must use `require'.
(use-package modus-themes
  :config
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-italic-constructs t
        )

  ;; Maybe define some palette overrides, such as by using our presets
  (setq modus-themes-common-palette-overrides
        modus-themes-preset-overrides-intense)

  (modus-themes-with-colors
    (custom-set-faces
     `(fill-column-indicator ((,c :height 1.0 :background ,bg-inactive :foreground ,bg-inactive)))))

  ;; Load the theme of your choice.
  (load-theme 'modus-operandi)

  (define-key global-map (kbd "<f5>") #'modus-themes-toggle))

(use-package fontaine
  :config
  (require 'fontaine)

  (setq fontaine-latest-state-file
	(locate-user-emacs-file "fontaine-latest-state.eld"))

  ;; Iosevka Comfy is my highly customised build of Iosevka with
  ;; monospaced and duospaced (quasi-proportional) variants as well as
  ;; support or no support for ligatures:
  ;; <https://github.com/protesilaos/iosevka-comfy>.
  (setq fontaine-presets
	'((small
           :default-family "UbuntuMono Nerd Font"
           :default-height 115
           :variable-pitch-family "Ubuntu")
          (regular) ; like this it uses all the fallback values and is named `regular'
          (medium
           :default-weight semilight
           :default-height 150
           :bold-weight extrabold)
          (large
           :inherit medium
           :default-height 180)
          (presentation
           :default-height 240)
          (t
           ;; I keep all properties for didactic purposes, but most can be
           ;; omitted.  See the fontaine manual for the technicalities:
           ;; <https://protesilaos.com/emacs/fontaine>.
           :default-family "UbuntuMono Nerd Font"
           :default-weight regular
           :default-height 150

           :fixed-pitch-family nil ; falls back to :default-family
           :fixed-pitch-weight nil ; falls back to :default-weight
           :fixed-pitch-height 1.0

           :fixed-pitch-serif-family nil ; falls back to :default-family
           :fixed-pitch-serif-weight nil ; falls back to :default-weight
           :fixed-pitch-serif-height 1.0

           :variable-pitch-family "Ubuntu"
           :variable-pitch-weight nil
           :variable-pitch-height 1.0

           :mode-line-active-family nil ; falls back to :default-family
           :mode-line-active-weight nil ; falls back to :default-weight
           :mode-line-active-height 0.9

           :mode-line-inactive-family nil ; falls back to :default-family
           :mode-line-inactive-weight nil ; falls back to :default-weight
           :mode-line-inactive-height 0.9

           :header-line-family nil ; falls back to :default-family
           :header-line-weight nil ; falls back to :default-weight
           :header-line-height 0.9

           :line-number-family nil ; falls back to :default-family
           :line-number-weight nil ; falls back to :default-weight
           :line-number-height 0.9

           :tab-bar-family nil ; falls back to :default-family
           :tab-bar-weight nil ; falls back to :default-weight
           :tab-bar-height 1.0

           :tab-line-family nil ; falls back to :default-family
           :tab-line-weight nil ; falls back to :default-weight
           :tab-line-height 1.0

           :bold-family nil ; use whatever the underlying face has
           :bold-weight bold

           :italic-family nil
           :italic-slant italic

           :line-spacing nil)))

  ;; Set the last preset or fall back to desired style from `fontaine-presets'
  ;; (the `regular' in this case).
  (fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular))

  ;; Persist the latest font preset when closing/starting Emacs and
  ;; while switching between themes.
  (fontaine-mode 1)

  ;; fontaine does not define any key bindings.  This is just a sample that
  ;; respects the key binding conventions.  Evaluate:
  ;;
  ;;     (info "(elisp) Key Binding Conventions")
  (define-key global-map (kbd "C-c f") #'fontaine-set-preset)
  )


(if (equal window-system 'x)
    (progn
      (use-package greader
        :config
        (setq greader-espeak-rate 300))
      (use-package elfeed
        :config
        (setq elfeed-feeds
              '(("http://nullprogram.com/feed/" emacs)
                ("https://planet.emacslife.com/atom.xml" emacs)
                ("https://www.reddit.com/r/emacs.rss" emacs)
                ("https://protesilaos.com/master.xml" emacs)
                ("https://sachachua.com/blog/feed" emacs)
                ("https://www.reddit.com/r/orgmode.rss" emacs)
                ("https://karthinks.com/index.xml" emacs)
                ("https://draculatheme.com/rss.xml" theme)))))
  )

(use-package treemacs
  :hook (after-init . treemacs)
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t t"   . treemacs)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag))
  :config
  )

(use-package toc-org
  )
(add-hook 'org-mode-hook 'toc-org-mode)

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  )

(define-key dired-mode-map (kbd "E")
	    (defun open-window-manager ()
	      "Open default system windows manager in current directory"
	      (interactive)
	      (save-window-excursion
		(if (equal window-system 'w32)
		    (async-shell-command "explorer .")
		  (if (equal window-system 'x)
		      (async-shell-command "thunar ."))))))

(require 'flyspell)
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

(if (equal window-system 'w32)
    (defcustom ispell-common-dictionaries '("en_US") "List of dictionaries for common use" :group 'ispell)
  (defcustom ispell-common-dictionaries '("en_US") "List of dictionaries for common use" :group 'ispell))

(setq-default ispell-dictionary (car ispell-common-dictionaries))

(define-key flyspell-mode-map (kbd "C-x M-4")
	    (defun flyspell-buffer-or-region ()
	      (interactive)
	      (if (region-active-p)
		  (flyspell-region (region-beginning) (region-end))
		(flyspell-buffer))))

(require 'ibuffer nil t)
;; ibuffer groups
(setq-default ibuffer-saved-filter-groups
	      (quote (("default"
		       ("org"  (mode . org-mode))
		       ("dired" (mode . dired-mode))
		       ("D" (mode . d-mode))
		       ("C/C++" (or
                                 (mode . cc-mode)
                                 (mode . c-mode)
                                 (mode . c++-mode)))
		       ("magit" (name . "^\\*magit"))
		       ("Markdown" (mode . markdown-mode))
		       ("emacs" (name . "^\\*Messages\\*$"))
		       ("shell commands" (name . "^\\*.*Shell Command\\*"))))))
(add-hook 'ibuffer-mode-hook
          (lambda ()
	    (ibuffer-switch-to-saved-filter-groups "default")))

(global-set-key (kbd "\C-x \C-b") 'ibuffer)

(define-key global-map "\C-c+"
	    (defun increment-decimal-number-at-point (&optional arg)
	      "Increment the number at point by `arg'."
	      (interactive "p*")
	      (save-excursion
		(save-match-data
		  (let (inc-by field-width answer)
		    (setq inc-by (if arg arg 1))
		    (skip-chars-backward "0123456789")
		    (when (re-search-forward "[0-9]+" nil t)
		      (setq field-width (- (match-end 0) (match-beginning 0)))
		      (setq answer (+ (string-to-number (match-string 0) 10) inc-by))
		      (when (< answer 0)
			(setq answer (+ (expt 10 field-width) answer)))
		      (replace-match (format (concat "%0" (int-to-string field-width) "d")
					     answer))))))))

(use-package olivetti
  )

(when (equal window-system 'x)
  (use-package notmuch))

(use-package casual
  :ensure t
  :bind (:map calc-mode-map ("C-o" . 'casual-calc-tmenu)))

(use-package casual-info
  :ensure t
  :bind (:map Info-mode-map ("C-o" . 'casual-info-tmenu)))

(use-package casual-dired
  :ensure t
  :bind (:map dired-mode-map ("C-o" . 'casual-dired-tmenu)))

(use-package casual-avy
  :ensure t
  :bind ("M-g" . casual-avy-tmenu))

(use-package cc-isearch-menu
  :ensure t
  :bind (:map isearch-mode-map ("<f2>" . 'cc-isearch-menu-transient)))

;; 1  (use-package casual-calc
;; 2    :ensure t
;; 3    :bind (:map calc-mode-map ("C-o" . #'casual-calc-tmenu)))
;; 4
;; 5  (use-package casual-isearch
;; 6    :ensure t
;; 7    :bind (:map isearch-mode-map ("<f2>" . #'casual-isearch-tmenu)))

(defun drr-insert-date-stamp-prefix ()
  "Inserts the current date in mm-dd-yyyy format, prefixed with 'Date: '."
  (interactive)
  (insert (format-time-string "Date: %m-%d-%Y")))

(defun drr-insert-date-stamp ()
  "Inserts the current date in mm-dd-yyyy format"
  (interactive)
  (insert (format-time-string "%m-%d-%Y")))

(defun drr-locate-current-file-in-explorer ()
  (interactive)
  (cond
   ;; In buffers with file name
   ((buffer-file-name)
    (shell-command (concat "start explorer /e,/select,\"" (replace-regexp-in-string "/" "\\\\" (buffer-file-name)) "\"")))
   ;; In dired mode
   ((eq major-mode 'dired-mode)
    (shell-command (concat "start explorer /e,\"" (replace-regexp-in-string "/" "\\\\" (dired-current-directory)) "\"")))
   ;; In eshell mode
   ((eq major-mode 'eshell-mode)
    (shell-command (concat "start explorer /e,\"" (replace-regexp-in-string "/" "\\\\" (eshell/pwd)) "\"")))
   ;; Use default-directory as last resource
   (t
    (shell-command (concat "start explorer /e,\"" (replace-regexp-in-string "/" "\\\\" default-directory) "\"")))))

(defun drr-my-reindent-file ()
  "Reindent the entire file and return to the original cursor position."
  (interactive)
  (let ((original-line (line-number-at-pos))
        (original-column (current-column)))
    (goto-char (point-min)) ; Move to the beginning of the file
    (mark-whole-buffer)    ; Select all lines
    (indent-region (point-min) (point-max)) ; Reindent the entire buffer (equivalent to C-M-\)
    (goto-line original-line) ; Return to the original line
    (move-to-column original-column nil) ; Return to the original column
    ))

(global-set-key (kbd "<pause>") 'drr-my-reindent-file) ; Bind to pause key

(repeat-mode 1)

(provide 'init-user)
