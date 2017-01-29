; list the repositories containing them
(setq package-archives '(("elpa" . "http://tromey.com/elpa/")
			 ("gnu" . "http://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")))

; activate all the packages (in particular autoloads)
(package-initialize)

; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

; list the packages you want
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
		      inf-ruby
		      ruby-test-mode))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(require 'better-defaults)

(setq inhibit-splash-screen t
      initial-scratch-message nil
      initial-major-mode 'ruby-mode)

;;(load-theme 'solarized-dark t)
(load-theme 'kooten t)

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
