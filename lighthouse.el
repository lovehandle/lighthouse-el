(require 'url)
(require 'json)
(require 'button)

(setq lighthouse-domains
      '(((domain . "your-domain.lighthouseapp.com")
         (token  . "your-lighthouse-token")))
      )

(defgroup lighthouse nil "lighthouse for Emacs"
  :group 'applications)


(defvar lighthouse-domains nil
  "An alist of domains that you can connect to. If set,
   you will be prompted for a domain when you try to to connect.)"
  )

(defcustom lighthouse-domain nil "The domain to connect to"
  :group 'lighthouse
  :type 'string)

(defcustom lighthouse-token nil "Your lighthouse token"
  :group 'lighthouse
  :type 'string)

(defun alist-value (key alist) (let ((a (assoc key alist))) (and a (cdr a))))

(defun node-from-key-and-value (value key alist)
  (let ((node (find-if
               (lambda (node)
                 (equal (cdr (assoc key node)) value))
               alist)))
    (and node)))

(defun lighthouse-url-retrieve-synchronously (url)
  (lighthouse-response (url-retrieve-synchronously url)))


(defun lighthouse-api-url (&optional format-string &rest args)
  (let ((base (concat "http://" lighthouse-domain)))
    (cond (format-string (concat base "/" (apply 'format (concat format-string "?_token=" lighthouse-token) args)))
          (t base))))

(defun lighthouse-completing-read (prompt choices  &optional predicate
                                          require-match initial-input
                                          hist def)
  "use ido if available"
  (if (functionp 'ido-completing-read)
      (ido-completing-read prompt choices predicate require-match
                           initial-input hist def)
    (completing-read prompt collection predicate
                     require-match initial-input hist def
                     inherit-input-method))
  )

(defun lighthouse-response (buffer)
  ""
  (unwind-protect
      (with-current-buffer buffer
        (save-excursion
          (url-http-parse-response)
          (url-http-parse-headers)
          (goto-char url-http-end-of-headers)
          (if (and (>= url-http-response-status 200)
                   (< url-http-response-status 300))
              (if (looking-at "\n{")
                  (let ((response (json-read)))
                    response)
                nil)
            (signal 'lighthouse-error
                    (list (format "Response code %d"
                                  url-http-response-status))))))
    (kill-buffer buffer)))

(defun lighthouse-select-domain ()
  (let ((domain-name
         (lighthouse-completing-read
          "Domain: "
          (map 'list
               (lambda (domain)
                 (alist-value 'domain domain))
               lighthouse-domains) nil t)))
    (or (find-if
         (lambda (domain)
           (message (alist-value 'domain domain))
           (equal (alist-value 'domain domain) domain-name))
         lighthouse-domains)
        (error 'no-such-domain)))
  )

(defun lighthouse-project-names ()
  (map 'list (lambda (project-details) (alist-value 'name project-details))
       lighthouse-projects))

(defun lighthouse-project-id-from-name (name)
  (alist-value 'id (node-from-key-and-value name 'name lighthouse-projects)))

(defun lighthouse-fetch-projects ()
  (let ((response
         (cdar
          (lighthouse-url-retrieve-synchronously
           (lighthouse-api-url "projects.json")))))
    (map 'list (lambda (projects)
                 (alist-value 'project projects)) response)))

(defun lighthouse-fetch-project-tickets ()
  (let ((response
         (cdar
          (lighthouse-url-retrieve-synchronously
           (lighthouse-api-url (concat "projects/"
                                       (number-to-string lighthouse-project-id)
                                       "/tickets.json"))))))
    (map 'list (lambda (tickets)
                 (alist-value 'ticket tickets)) response)))

(defun lighthouse-fetch-project-ticket (ticket-id)
  (let ((response
         (cdar
          (lighthouse-url-retrieve-synchronously
           (lighthouse-api-url (concat "projects/"
                                       (number-to-string lighthouse-project-id)
                                       "/tickets/" ticket-id ".json"))))))
    (alist-value 'ticket response))
  )

(defun lighthouse-project-ticket-ids ()
  (map 'list (lambda (ticket)
               (number-to-string (alist-value 'number ticket))) lighthouse-project-tickets))

(defun lighthouse-project-ticket-from-id (id)
  (node-from-key-and-value (string-to-number id) 'number lighthouse-project-tickets))

(defun lighthouse-fetch-project-users ()
  (let ((response
         (cdar
          (lighthouse-url-retrieve-synchronously
           (lighthouse-api-url (concat "projects/" (number-to-string
                                                    lighthouse-project-id)
                                       "/memberships.json"))))))
    (map 'list (lambda (users)
                 (alist-value 'user users))
         (map 'list (lambda (memberships)
                      (alist-value 'membership memberships)) response))
    ))

(defun lighthouse-project-user-name-from-id (id)
  (alist-value 'name (node-from-key-and-value (string-to-number id) 'id lighthouse-project-users)))


(defun lighthouse ()
  (interactive)
  (if lighthouse-domains
      (let ((domain (lighthouse-select-domain)))
        (setq lighthouse-domain (alist-value 'domain domain)
              lighthouse-token  (alist-value 'token  domain)
              lighthouse-projects (lighthouse-fetch-projects)
              lighthouse-project-name (lighthouse-completing-read
                                       "Lighthouse projects: "
                                       (lighthouse-project-names) nil t)
              lighthouse-project-id (lighthouse-project-id-from-name
                                     lighthouse-project-name)
              lighthouse-project-tickets (lighthouse-fetch-project-tickets)
              lighthouse-project-users (lighthouse-fetch-project-users)))
    )
  (lighthouse-get-buffer-create)
  )

(defun lighthouse-ticket ()
  (interactive)
  (let ((ticket-id
         (lighthouse-completing-read
          "Ticket Number: "
          (lighthouse-project-ticket-ids) nil t)))
    (lighthouse-display-ticket
     (lighthouse-project-ticket-from-id ticket-id))))

(defun lighthouse-get-buffer-create ()
  (let ((buffer (get-buffer-create (concat "*Lighthouse: "
                                           lighthouse-project-name "*"))))
    (switch-to-buffer buffer)
    (kill-all-local-variables)
    (lighthouse-project-mode)
    (lighthouse-display-tickets))
  )

(defun lighthouse-display-tickets ()
  (map 'list (lambda (ticket)
               (lighthouse-display-ticket-summary ticket))
       lighthouse-project-tickets))


(defun lighthouse-display-ticket-summary (ticket)
  ""
(let ((start (point))
      (title (alist-value 'title ticket))
      (id (alist-value 'number ticket))
      (state (alist-value 'state ticket))
      (assigned (alist-value 'assigned_user_name ticket)))
  (insert (concat (number-to-string id) " " state " " title " " assigned "\n"))
  (make-button start (+ start 1)
               'action 'lighthouse-display-ticket
               'ticket ticket))
)

(defun lighthouse-display-ticket (x)
  ""
  )

(define-derived-mode lighthouse-project-mode text-mode "Lighthouse room mode"
  "Major mode for lighthouse projects"
  (make-local-variable 'lighthouse-domain)
  (make-local-variable 'lighthouse-token)
  (make-local-variable 'lighthouse-projects)
  (make-local-variable 'lighthouse-project-name)
  (make-local-variable 'lighthouse-project-id)
  (make-local-variable 'lighthouse-project-tickets)

  (define-key lighthouse-project-mode-map (kbd "C-c C-t") 'lighthouse-ticket))

(provide 'lighthouse)
