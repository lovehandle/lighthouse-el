;;Lighthouse for Emacs


(defgroup lighthouse nil "Lighthouse for Emacs"
  :group 'applications)

(defgroup lighthouse-faces nil "Lighthouse faces."
  :group 'lighthouse)

(defvar lighthouse-accounts nil
  "An alist of Lighthouse accounts you can connect to. (If set,
   you will be prompted for a domain when you try to connect.)")

(defcustom lighthouse-account nil
  "Your lighthouse account name."
  :type '(choice (const :tag "Ask" nil) (string))
  :group 'lighthouse)

(defcustom lighthouse-api-token nil
  "Your lighthouse api token."
  :type '(choice (const :tag "Ask" nil) (string))
  :group 'lighthouse)


(defun lighthouse-select-account ()
  (interactive)
  (let ((account (ido-completing-read
                  "Domain: " (map 'list
                                  (lambda (domain)
                                    (alist-value 'domain domain))
                                  lighthouse-accounts) nil t))))
  (error 'account-not-found))

(defun lighthouse-projects ()
  (let ((projects (plist-get (lighthouse-retrieve-synchronously (lighthouse-projects-url)) :projects)))
    (map-values :project projects)))

(defun lighthouse-tickets (project-id)
  (let ((tickets (plist-get (lighthouse-retrieve-synchronously (lighthouse-tickets-url project-id)) :tickets)))
    (map-values :ticket tickets)))

(defun lighthouse-ticket (project-id ticket-id)
  (plist-get (lighthouse-retrieve-synchronously (lighthouse-ticket-url project-id ticket-id)) :ticket))

(require 'lighthouse-ticket)

(provide 'lighthouse)
