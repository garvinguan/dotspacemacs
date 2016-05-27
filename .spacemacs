;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load. If it is the symbol `all' instead
   ;; of a list then all discovered layers will be installed.
   dotspacemacs-configuration-layers
   '(
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
     ;; <M-m f e R> (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     auto-completion
     ;; better-defaults
     emacs-lisp
     (javascript :variables tern-command '("node" "c:/Users/Garvin/AppData/Roaming/npm/node_modules/tern/bin/tern"))
     git
     go
     ;; markdown
     org
     private
     scala
     ;; (shell :variables
     ;;        shell-default-height 30
     ;;        shell-default-position 'bottom)
     ;; spell-checking
     ;; syntax-checking
     ;; version-control
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '(
                                      beacon
                                      one-time-pad-encrypt
                                      hlm-flx
                                      key-chord
                                      company
                                      go-mode company-go
                                      popwin
                                      )
   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages '(
                                    yasnippet
                                    )
   ;; If non-nil spacemacs will delete any orphan packages, i.e. packages that
   ;; are declared in a layer which is not a member of
   ;; the list `dotspacemacs-configuration-layers'. (default t)
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https nil
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 5
   ;; If non nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. (default t)
   dotspacemacs-check-for-update t
   ;; One of `vim', `emacs' or `hybrid'. Evil is always enabled but if the
   ;; variable is `emacs' then the `holy-mode' is enabled at startup. `hybrid'
   ;; uses emacs key bindings for vim's insert mode, but otherwise leaves evil
   ;; unchanged. (default 'vim)
   dotspacemacs-editing-style 'vim
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official
   ;; List of items to show in the startup buffer. If nil it is disabled.
   ;; Possible values are: `recents' `bookmarks' `projects'.
   ;; (default '(recents projects))
   dotspacemacs-startup-lists '(recents projects)
   ;; Number of recent files to show in the startup buffer. Ignored if
   ;; `dotspacemacs-startup-lists' doesn't include `recents'. (default 5)
   dotspacemacs-startup-recent-list-size 5
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(spacemacs-dark
                         spacemacs-light
                         solarized-light
                         solarized-dark
                         leuven
                         monokai
                         zenburn)
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font. `powerline-scale' allows to quickly tweak the mode-line
   ;; size to make separators look not too crappy.
   dotspacemacs-default-font '("Source Code Pro"
                               :size 13
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m)
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for separate commands under <C-i>
   ;; and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; (Not implemented) dotspacemacs-distinguish-gui-ret nil
   ;; The command key used for Evil commands (ex-commands) and
   ;; Emacs commands (M-x).
   ;; By default the command key is `:' so ex-commands are executed like in Vim
   ;; with `:' and Emacs commands are executed with `<leader> :'.
   dotspacemacs-command-key ":"
   ;; If non nil `Y' is remapped to `y$'. (default t)
   dotspacemacs-remap-Y-to-y$ t
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non nil then the last auto saved layouts are resume automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   ;; If non nil then `ido' replaces `helm' for some commands. For now only
   ;; `find-files' (SPC f f), `find-spacemacs-file' (SPC f e s), and
   ;; `find-contrib-file' (SPC f e c) are replaced. (default nil)
   dotspacemacs-use-ido nil
   ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-micro-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.1
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar nil
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters the
   ;; point when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; If non nil line numbers are turned on in all `prog-mode' and `text-mode'
   ;; derivatives. If set to `relative', also turns on relative line numbers.
   ;; (default nil)
   dotspacemacs-line-numbers t
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil advises quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed'to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup 'trailing
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."
  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place you code here."
  (setq large-file-warning-threshold 15000000)
  (use-package org
    :bind (("C-c l" . org-store-link)
           ("C-c a" . org-agenda)
           ("C-c c" . org-capture))
    :config
    (require 'org-drill)
    (setq org-drill-add-random-noise-to-intervals-p t)
    (add-hook #'org-mode-hook #'org-indent-mode)
    (setq org-default-notes-file (concat org-directory "/notes.org"))
    (setq org-log-done t)
    (setq org-capture-templates
          '(("i" "importanttodo" entry (file+headline "~/org/notes.org" "Important")
             "* TODO  %?\n  %U\n")
            ("w" "websitetodo" entry (file+headline "~/org/website.org" "What to work on next")
             "* TODO  %?")
            ("n" "interesting things to find out" entry (file+headline "~/org/notes.org" "What to find out next")
             "* TODO  %?")
            ("q" "questions" entry (file+headline "~/org/notes.org" "Questions to ask")
             "* TODO  %?\n  %U\n  %a")
            ("r" "reminder" entry (file+headline "~/org/notes.org" "Reminder")
             "* TODO  %?\n  %U\n  %a")
            ("o" "other" plain (file+headline "~/org/notes.org" "Other")
             "%?\n")
            ("g" "go notes" entry (file+headline "~/org/notes.org" "Go notes")
             "*  %?\n  %U\n  %a")
            ("l" "journal" entry (file+datetree "~/org/journal.org")
             "*  %?\n %U\n  %a")
            ("j" "Japanese Word" entry (file+headline "~/reading/skip/japanese.org" "Words")
             "* <[%(garvin/japanese-prompt)]> :drill:
Definition:
%(garvin/japanese-get-definition (garvin/japanese-dict-find garvin/japanese-word))
** Characters
%(garvin/japanese-get-word garvin/japanese-word-dict)
** Pronunciation
%(garvin/japanese-get-pronunciation garvin/japanese-word-dict)
")))
    (setq org-agenda-files (list "~/org/website.org"
                                 "~/org/other.org"
                                 "~/org/extensiontodos.org"
                                 ;; "~/org/recommendations.org"
                                 "~/org/notes.org"
                                 )))
  (remove-hook 'prog-mode-hook #'smartparens-mode)
  (use-package popwin
    :commands popwin-mode
    :init (popwin-mode 1)
    :config
    (defvar popwin:special-display-config-backup popwin:special-display-config)
    ;;    (setq display-buffer-function 'popwin:display-buffer)
    (setq popwin:special-display-config nil)
    ;; basic
    (push '("*Help*" :noselect t :stick t) popwin:special-display-config)
    (push '("*Shell Command Output*" :noselect t :stick t) popwin:special-display-config)

    ;; compilation
    (push '(compilation-mode :stick t :width 0.4) popwin:special-display-config)
    (push '("*Compile-Log*" :stick t :noselect t) popwin:special-display-config)

    ;; magit
    (push '("*magit-process*" :stick t) popwin:special-display-config)

    ;; man
    (push '(Man-mode :stick t :height 20) popwin:special-display-config)

    ;; Elisp
    (push '("*ielm*" :stick t) popwin:special-display-config)
    (push '("*eshell pop*" :stick t) popwin:special-display-config)

    ;; python
    (push '("*Python*"   :stick t) popwin:special-display-config)
    (push '("*Python Help*" :stick t :height 20) popwin:special-display-config)
    (push '("*jedi:doc*" :stick t :noselect t) popwin:special-display-config)

    (push '("*Occur*" :stick t) popwin:special-display-config)

    ;; org-mode
    (push '("*Org tags*" :stick t :height 30) popwin:special-display-config)

    ;; Completions
    (push '("*Completions*" :stick t :noselect t) popwin:special-display-config)
    )
  (use-package key-chord
    :config
    (key-chord-mode 1)
    (setq key-chord-two-keys-delay .040
          key-chord-one-key-delay .135)
    (key-chord-define-global "jl" 'helm-find-files)
    (key-chord-define-global ";s" 'save-buffer)
    (key-chord-define-global "ef" 'eval-buffer)
    (key-chord-define-global "kl" 'helm-mini)
    (key-chord-define-global ";w" 'other-window)
    ;; Make movement keys work like they should
    (define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
    (define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
    (define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
    (define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
    ;; Make horizontal movement cross lines
    (setq-default evil-cross-lines t)
    (define-key evil-motion-state-map (kbd "SPC") #'evil-ace-jump-word-mode)
    (define-key evil-motion-state-map (kbd "C-i") 'evil-jump-forward)
    (key-chord-define evil-normal-state-map "jk" 'evil-force-normal-state)
    (key-chord-define evil-visual-state-map "jk" 'evil-change-to-previous-state)
    (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
    (key-chord-define evil-replace-state-map "jk" 'evil-normal-state)

    (defun normal-state-after-save ()
      "Return to normal mode after saving"
      (interactive)
      (save-buffer)
      (evil-normal-state))
    (evil-define-key 'normal org-mode-map (kbd "o") 'org-evil-open-below)
    (evil-define-key 'normal org-mode-map (kbd "O") 'org-evil-open-above)
    (key-chord-define evil-insert-state-map ";s" 'normal-state-after-save))

  (use-package beacon
    :defer t
    :diminish beacon-mode
    :init (beacon-mode 1)
    :config
    (setq beacon-blink-duration 0.2)
    (setq beacon-blink-delay 0.1))

  (use-package helm
    :diminish helm-mode
    :config
    (helm-mode 1)
    (helm-projectile-on)
    (spacemacs/set-leader-keys "pk" 'helm-projectile-find-file-in-known-projects)
    (bind-keys :map helm-map
               ("C-f" . helm-execute-persistent-action)))

  (use-package projectile
    :defer 5
    :commands projectile-global-mode
    :diminish projectile-mode
    :config
    (bind-key "C-c p b" #'projectile-switch-to-buffer #'projectile-command-map)
    (bind-key "C-c p K" #'projectile-kill-buffers #'projectile-command-map)

    ;; global ignores
    (add-to-list 'projectile-globally-ignored-files ".tern-port")
    (add-to-list 'projectile-globally-ignored-files "GTAGS")
    (add-to-list 'projectile-globally-ignored-files "GPATH")
    (add-to-list 'projectile-globally-ignored-files "GRTAGS")
    (add-to-list 'projectile-globally-ignored-files "GSYMS")
    (add-to-list 'projectile-globally-ignored-files ".DS_Store")
    ;; always ignore .class files
    (add-to-list 'projectile-globally-ignored-file-suffixes ".class"))

  (use-package helm-swoop
    :bind (("M-o" . helm-swoop)
           ("M-O" . helm-swoop-back-to-last-point)
           ("C-c M-o" . helm-multi-swoop))
    :config
    ;; When doing isearch, hand the word over to helm-swoop
    (define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
    ;; From helm-swoop to helm-multi-swoop-all
    (define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)
    ;; Save buffer when helm-multi-swoop-edit complete
    (setq helm-multi-swoop-edit-save t
          ;; If this value is t, split window inside the current window
          helm-swoop-split-with-multiple-windows nil
          ;; Split direcion. 'split-window-vertically or 'split-window-horizontally
          helm-swoop-split-direction 'split-window-horizontally
          ;; If nil, you can slightly boost invoke speed in exchange for text color
          helm-swoop-speed-or-color nil))

  (use-package erc
    :init
    (setq erc-nick "workisfun"
          erc-hide-list '("JOIN" "PART" "QUIT"))
    (setq erc-autojoin-channels-alist
          '(("freenode.net" "#emacs-beginners" "#go-nuts" "#javascript")))
    :config
    (spacemacs/set-leader-keys "ag" 'garvin-erc)
    )

  (use-package ediff
    :config
    (progn
      (setq
       ;; Always split nicely for wide screens
       ediff-split-window-function 'split-window-horizontally)))

  (use-package magit
    :bind (("M-g M-g" . magit-status)
           ("C-x g" . magit-status))
    :init (add-hook 'magit-mode-hook 'hl-line-mode)
    :config
    (setenv "GIT_ASKPASS" "git-gui--askpass")
    (setenv "GIT_PAGER" "")
    (if (file-exists-p  "/usr/local/bin/emacsclient")
        (setq magit-emacsclient-executable "/usr/local/bin/emacsclient")
      (setq magit-emacsclient-executable (executable-find "emacsclient")))
    (defun my/magit-browse ()
      "Browse to the project's github URL, if available"
      (interactive)
      (let ((url (with-temp-buffer
                   (unless (zerop (call-process-shell-command
                                   "git remote -v" nil t))
                     (error "Failed: 'git remote -v'"))
                   (goto-char (point-min))
                   (when (re-search-forward
                          "github\\.com[:/]\\(.+?\\)\\.git" nil t)
                     (format "https://github.com/%s" (match-string 1))))))
        (unless url
          (error "Can't find repository URL"))
        (browse-url url)))

    (define-key magit-mode-map (kbd "C-c C-b") #'my/magit-browse))

  (use-package helm-flx
    :init (helm-flx-mode +1))

  (use-package go-mode
    :config
    (use-package go-mode-autoloads)
    (use-package company-go)
    (add-hook 'go-mode-hook (lambda ()
                              (set (make-local-variable 'company-backends) '(company-go))
                              (company-mode))))
  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values (quote ((encoding . japanese-iso-8bit)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))
