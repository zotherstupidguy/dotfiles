;; Mohamed Fouad
;; zotherstupidguy@gmail.com
;; hackspree.com

;; ref: https://mixandgo.com/blog/how-ive-convinced-emacs-to-dance-with-ruby ( A lot need to be taken from this Tut, very gooooooooooood)
;; ref: https://martinralbrecht.wordpress.com/2014/11/03/c-development-with-emacs/
;; ref: https://martinralbrecht.wordpress.com/2015/02/12/sage-development-with-emacs/ 
;; ref: http://howardism.org/Technical/Emacs/literate-programming-tutorial.html 
;; ref: https://ayueer.wordpress.com/2006/07/01/some-emacs-tricks-on-ruby/ 
;; ref: http://worace.works/2016/06/07/getting-started-with-emacs-for-ruby/
;; ref: http://pragmaticemacs.com/emacs/advanced-undoredo-with-undo-tree/

;; list the repositories containing them
(setq package-archives '(("elpa" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))

;; activate all the packages (in particular autoloads)
(package-initialize)

;; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

;; list the packages you want
(setq package-list '(better-defaults
                     kooten-theme ;;solarized-theme
                     ;;helm
                     ;;helm-projectile
                     ;;helm-ag
                     evil
                     key-chord
                     restart-emacs ;; M-x restart-emacs
                     ruby-electric
                     seeing-is-believing
                     ;;chruby ;;rbenv
                     yasnippet           
                     inf-ruby
                     ess
                     flycheck
                     company ;; https://company-mode.github.io/
                     ;;magit ;; one-thing-per-commit
                     ;;magit-popup
                     git-timemachine ;; step forward and backward through the history of a file
                     highlight-indentation  ;;TODO require it and use it for python projects
                     openwith ;; open links for files and webpages into external programs 
                     robe ;; required for robe-mode-hook https://github.com/dgutov/robe
                     org-pomodoro ;; https://github.com/lolownia/org-pomodoro
                     pos-tip  ;; https://www.topbug.net/blog/2016/11/03/emacs-display-function-or-variable-information-near-point-cursor/
                     mew ;; email in emacs world 
                     elfeed  ;; emacs RSS
                     ruby-test-mode))

;; Somewhere in your .emacs file
(setq elfeed-feeds
      '("https://www.upwork.com/ab/feed/jobs/rss?contractor_tier=1&verified_payment_only=1&q=rails&api_params=1&securityToken=1ef0bfc31f6c4d62393591a90cf9ae62a8c932b0279904de87c1fcb1f112c4dc7ce9e9b92a90baf1795d27a262b823bc7b14e80efb733f3ef4b783d4f4fa9cbc&userUid=743463620975747072&orgUid=743463620979941377"
        "https://www.upwork.com/ab/feed/jobs/rss?contractor_tier=1&verified_payment_only=1&q=rails&api_params=1&securityToken=1ef0bfc31f6c4d62393591a90cf9ae62a8c932b0279904de87c1fcb1f112c4dc7ce9e9b92a90baf1795d27a262b823bc7b14e80efb733f3ef4b783d4f4fa9cbc&userUid=743463620975747072&orgUid=743463620979941377"
        "http://www.terminally-incoherent.com/blog/feed/"))

;; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(require 'better-defaults)

(setq inhibit-splash-screen t
      initial-scratch-message nil
      initial-major-mode 'ruby-mode)

;; (load-theme 'solarized-dark t)
(load-theme 'kooten t)

;; flycheck
;; ref: http://www.flycheck.org/  
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

;; company-mode
;; Completion will start automatically after you type a few letters.
;; Use M-n and M-p to select, <return> to complete or <tab> to complete the common part.
;; Search through the completions with C-s, C-r and C-o. Press M-(digit) to quickly complete with one of the first 10 candidates.
;; ref:  https://company-mode.github.io/
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)


;; A Git Porcelain inside Emacs
;;(require 'magit)
;;(global-set-key (kbd "<f7>") 'magit-status)

;; git-timemachine step forward and backward through the history of a file
;; p Visit previous historic version
;; n Visit next historic version
;; w Copy the abbreviated hash of the current historic version
;; W Copy the full hash of the current historic version
;; g Goto nth revision
;; q Exit the time machine. 
;; ref: https://github.com/pidu/git-timemachine 
(require 'git-timemachine)

;; yasnippet, docs: http://joaotavora.github.io/yasnippet/
;;TODO create some custom org-snippet for note-taking
(require 'yasnippet)
(yas-global-mode 1)

;; openwith for opening files in their respective external programs
(require 'openwith)
(openwith-mode t)
(setq openwith-associations '(("\\.pdf\\'" "evince" (file))))

;; org-mode configs
;; <s + tab to add codeblocks in org-mode
;; C-c' to edit codeblocks in their respective modes (C-c' to exit and go back to org-mode)
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(setq org-confirm-babel-evaluate nil
      org-src-fontify-natively t
      org-src-tab-acts-natively t)
;; enables org-babel for the mentioned languages; 
(org-babel-do-load-languages
 'org-babel-load-languages
 '( (perl . t)         
    (C . t)
    (ruby . t)
    (sh . t)
    (python . t)
    (latex . t)
    (emacs-lisp . t)   
    ))
;; org-mode table behaviour to all other modes
;; ref: http://orgmode.org/manual/Orgtbl-mode.html#Orgtbl-mode
(add-hook 'message-mode-hook 'turn-on-orgtbl)

;; emacs-speaks-statistics
(require 'ess)

;; Show line numbers
(global-linum-mode)
(setq linum-format "%d ")
(setq-default left-fringe-width  10)
;;(setq-default right-fringe-width  0)
;;(set-face-attribute 'fringe nil :background "yellow")


;; activate evil mode
(require 'evil)
(evil-mode 1)

;; Typography
;;(set-face-attribute 'default nil
;;	    :family "Source Code Pro"
;;	    :height 150
;;	    :weight 'normal
;;	    :width 'normal)

;;(require 'helm)
;;(require 'helm-projectile)
;;(require 'helm-ag)
;;(global-set-key (kbd "M-x") #'helm-M-x)
;;(global-set-key (kbd "s-f") #'helm-projectile-ag)
;;(global-set-key (kbd "s-t") #'helm-projectile-find-file-dwim)

;; Autoclose paired syntax elements like parens, quotes, etc
(require 'ruby-electric)
(add-hook 'ruby-mode-hook 'ruby-electric-mode)
;;(chruby "2.2.2");;TODO maybe use rbenv

;;Seeing Is Believing gives us:
;;C-. s - Run Seeing is Believing for the entire file
;;C-. c - Clear the Seeing is Believing output
;;C-. t - Tag a line to be "targeted" for evaluation by SiB
;;C-. x - Run only the "tagged" lines (those with trailing "# =>" markers)
(setq seeing-is-believing-prefix "C-.")
(add-hook 'ruby-mode-hook 'seeing-is-believing)
(require 'seeing-is-believing)

;; inf-ruby
;;Use C-c C-s to launch the inf-ruby process.
;;Use C-x o to switch to the inf-ruby pane and try running some random ruby snippets as you normally would from IRB or pry.
;;Go back to your Ruby buffer, select (by highlighting) a chunk of code, and use C-c C-r to push that Ruby code into the IRB session.
;;For example, try defining a class in your Ruby buffer, select the whole buffer, run C-c C-r, then swap over to the inf-ruby buffer and instantiate an instance of your class. Pretty cool!
;;Alternatively, use C-c M-r to run a selected chunk of code and automatically go to the ruby buffer
;;Finally, use helm-M-x (which we bound earlier to the default M-x keybinding) to search for âruby sendâ and see what other default bindings inf-ruby gives us.
;;If you do a lot of work in Rails or Sinatra, check out the commands inf-ruby-console-rails and inf-ruby-console-racksh. Using these commands inf-ruby can start a console session in the environment of your web project.
(autoload 'inf-ruby-minor-mode "inf-ruby" "Run an inferior Ruby process" t)
(add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)

;; ruby-test-mode
(require 'ruby-test-mode)
(add-hook 'ruby-mode-hook 'ruby-test-mode)
;; a smooth hook for ruby-test-mode
(add-hook 'compilation-finish-functions
          (lambda (buf strg)
            (switch-to-buffer-other-window "*compilation*")
            (read-only-mode)
            (goto-char (point-max))
            (local-set-key (kbd "q")
                           (lambda () (interactive) (quit-restore-window)))))
;;NOTE added by me
;;(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

;;Exit insert mode by pressing k and then j quickly
(setq key-chord-two-keys-delay 0.5)
(key-chord-define evil-insert-state-map "kj" 'evil-normal-state)
(key-chord-mode 1)

;; M-x simpleclip-paste and simpleclip-copy to interact with the clipboard
(setq x-select-enable-clipboard t)

;; add linenumber
(global-linum-mode t)

;; open emacs on fullscreen and maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; saves the minibuffer history
(savehist-mode 1)

;; shift + enter to add a newline under
(defun end-of-line-and-indented-new-line ()
  (interactive)
  (end-of-line)
  (newline-and-indent))
(global-set-key (kbd "<S-return>") 'end-of-line-and-indented-new-line)

;; automagically does a "chmod u+x" when you save a script file (starting with "#!").
;; Works with every kind of script, not only ruby ones. Just add that into .emacs
(add-hook 'after-save-hook
          '(lambda ()
             (progn
               (and (save-excursion
                      (save-restriction
                        (widen)
                        (goto-char (point-min))
                        (save-match-data
                          (looking-at "^#!"))))
                    (shell-command (concat "chmod u+x " 
                                           buffer-file-name))
                    (message (concat "Saved as script: 
   " buffer-file-name))))))

;; create directories if they don't exist while saving a file via C-x C-s
(add-hook 'before-save-hook
          (lambda ()
            (when buffer-file-name
              (let ((dir (file-name-directory buffer-file-name)))
                (when (and (not (file-exists-p dir))
                           (y-or-n-p (format "Directory %s does not exist. Create it?" dir)))
                  (make-directory dir t))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; undo tree mode                                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;turn on everywhere
(global-undo-tree-mode 1)
;; make ctrl-z undo
(global-set-key (kbd "C-z") 'undo)
;; make ctrl-Z redo
(defalias 'redo 'undo-tree-redo)
(global-set-key (kbd "C-S-z") 'redo)

;; http://orgmode.org/worg/org-tutorials/org-plot.html
(local-set-key "\M-\C-g" 'org-plot/gnuplot)

;; ElDoc - part of emacs
;; https://www.emacswiki.org/emacs/ElDoc
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
                                        ;(defun my-eldoc-display-message (format-string &rest args)
                                        ;  "Display eldoc message near point."
                                        ;  (when format-string
                                        ;    (pos-tip-show (apply 'format format-string args))))
                                        ;(setq eldoc-message-function #'my-eldoc-display-message)

;; https://github.com/dgutov/robe
(add-hook 'ruby-mode-hook 'robe-mode)
(eval-after-load 'company
  '(push 'company-robe company-backends))

;; Turn on eldoc in ruby files to display info about the
;; method or variable at point
(add-hook 'ruby-mode-hook 'eldoc-mode)

;; Display Time
(display-time-mode 1)
(defface egoge-display-time
  '((((type x w32 mac))
     ;; #060525 is the background colour of my default face.
     (:foreground "#060525" :inherit bold))
    (((type tty))
     (:foreground "blue")))
  "Face used to display the time in the mode line.")
;; This causes the current time in the mode line to be displayed in
;; `egoge-display-time-face' to make it stand out visually.
(setq display-time-string-forms
      '((propertize (concat " " 24-hours ":" minutes " ")
                    'face 'egoge-display-time)))

(add-hook 'ruby-mode-hook
          (lambda ()
            (hs-minor-mode 1) ;; Enables folding
            (modify-syntax-entry ?: "."))) ;; Adds ":" to the word definition

;; erc - emacs IRC
(setq erc-log-channels-directory "~/.erc/logs/")

;; log files automatically written when you part a channel (or quit), add the following line:
(setq erc-save-buffer-on-part t)

;; (defadvice save-buffers-kill-emacs (before save-logs (arg) activate)
;; (save-some-buffers t (lambda () (when (eq major-mode 'erc-mode) t))))

;; log on channel activity
;; (setq erc-save-buffer-on-part nil
  ;;     erc-save-queries-on-quit nil
    ;;   erc-log-write-after-send t
      ;; erc-log-write-after-insert t)


;; Cycle Buffers
(global-set-key [C-right] 'next-buffer)
(global-set-key [C-left] 'previous-buffer)

;; Cycling Windows
;; (global-set-key [S-left] 'windmove-left)          ; move to left window
;; (global-set-key [S-right] 'windmove-right)        ; move to right window
;; (global-set-key [S-up] 'windmove-up)              ; move to upper window
;; (global-set-key [S-down] 'windmove-down)          ; move to lower window
