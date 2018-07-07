;; Time-stamp: <2018-07-07 18:07:13 csraghunandan>

;; Copyright (C) 2016-2018 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghunandan <rnraghunandan@gmail.com>

;; ERC: the irc client for emacs
(use-package erc
  :ensure nil
  :bind (("C-c e" . my/erc-start-or-switch))
  :hook ((erc-send-pre . my/erc-preprocess))
  :config
  ;; don't show messages when a users quits or joins
  (setq erc-hide-list '("PART" "QUIT" "JOIN"))
  ;; defaults for ERC mode
  (setq erc-server "irc.freenode.net"
        erc-nick "CSRaghunandan")

  (when (executable-find "terminal-notifier")
    ;; to enable notifications for ERC via terminal-notifier
    (defvar erc-terminal-notifier-command nil "The path to terminal-notifier.")
    (setq erc-terminal-notifier-command (executable-find "terminal-notifier"))

    (defun erc-terminal-notifier-notify (title message)
      "Show a message with `terminal-notifier-command`."
      (start-process "terminal-notifier"
                     "*terminal-notifier*"
                     erc-terminal-notifier-command
                     "-title" title
                     "-message" message
                     "-activate" "org.gnu.Emacs"
                     "-sender" "org.gnu.Emacs"
                     "-reply"))

    (defun erc-terminal-notifier-text-matched (match-type nick message)
      "Show a notification, when user's nick is mentioned."
      (when (eq match-type 'current-nick)
        (unless (posix-string-match "^\\** *Users on #" message)
          (erc-terminal-notifier-notify
           (concat "ERC " (buffer-name (current-buffer)))
           (concat "\\<" (nth 0 (erc-parse-user nick)) "> " message)))))

    (if (is-mac-p)
        (add-hook 'erc-text-matched-hook 'erc-terminal-notifier-text-matched)))

  ;; set directory to save erc log files
  (setq erc-log-channels-directory (concat user-home-directory ".erc/logs/"))

  ;; if log file exists, load it
  (setq erc-log-insert-log-on-open t)

  ;; Kill buffers for channels after /part
  (setq erc-kill-buffer-on-part t)
  ;; Kill buffers for private queries after quitting the server
  (setq erc-kill-queries-on-quit t)
  ;; Kill buffers for server messages after quitting the server
  (setq erc-kill-server-buffer-on-quit t)

  ;; truncate long irc buffers
  (erc-truncate-mode +1)

  ;; use sensible names for irc buffers
  (setq erc-rename-buffers t)
  ;; Interpret mIRC-style color commands in IRC chats
  (setq erc-interpret-mirc-color t)

  (setq erc-autojoin-timing 'ident)
  (setq erc-fill-function 'erc-fill-static)
  (setq erc-fill-static-center 22)

  ;; hide messsages when lurkers join or quit
  (setq erc-lurker-hide-list (quote ("JOIN" "PART" "QUIT")))
  (setq erc-lurker-threshold-time 43200)

  (defun my/erc-preprocess (string)
    "Avoids channel flooding."
    (setq str
	      (string-trim
	       (replace-regexp-in-string "\n+" " " str))))

  (defun my/erc-start-or-switch ()
    "Connects to ERC, or switch to last active buffer."
    (interactive)
    (if (get-buffer "irc.freenode.net:6667")
	    (erc-track-switch-buffer 1)
	  (when (y-or-n-p "Start ERC? ")
	    (erc :server "irc.freenode.net" :port 6667 :nick "rememberYou"))))

  (defun my/erc-count-users ()
    "Displays the number of users connected on the current channel."
    (interactive)
    (if (get-buffer "irc.freenode.net:6667")
	    (let ((channel (erc-default-target)))
	      (if (and channel (erc-channel-p channel))
		      (message "%d users are online on %s"
			           (hash-table-count erc-channel-users)
			           channel)
	        (user-error "The current buffer is not a channel")))
	  (user-error "You must first start ERC"))))

;; erc-image: Fetch and show received images in a ERC buffer
;; https://github.com/kidd/erc-image.el
(use-package erc-image
  :after erc
  :config
  (add-to-list 'erc-modules 'image)
  (erc-update-modules))

;; erc-hl-nicks: Nickname Highlighting for ERC
;; https://github.com/leathekd/erc-hl-nicks/tree/master
(use-package erc-hl-nicks
  :after erc)

(provide 'setup-erc)

;; erc
;; press `C-c C-p' to quit erc channel
;; press `C-c C-l' to save the erc chat to log
;; if there are any logs, they will be restored automatically upon login
