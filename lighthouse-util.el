(require 'furl)
(require 'json)


(defun lighthouse-base-url ()
  (concat "http://" lighthouse-account ".lighthouseapp.com"))

(defun lighthouse-projects-url ()
  (concat (lighthouse-base-url) "/projects"))

(defun lighthouse-tickets-url (project-id)
  (concat (lighthouse-projects-url) "/" (number-to-string project-id) "/tickets"))

(defun lighthouse-ticket-url (project-id ticket-id)
  (concat (lighthouse-tickets-url project-id) "/" (number-to-string ticket-id)))

(defun lighthouse-project-memberships-url (project-id)
  (concat (lighthouse-projects-url) "/" (number-to-string project-id) "/memberships"))

(defun lighthouse-user-url (user-id)
  (concat (lighthouse-base-url) "/users" (number-to-string user-id)))

(defun lighthouse-user-memberships-url (user-id)
  (concat (lighthouse-user-url (number-to-string user-id)) "/memberships" ))

(defun lighthouse-milestones-url (project-id)
  (concat (lighthouse-projects-url) "/" (number-to-string project-id) "/milestones"))

(defun lighthouse-milestone-url (project-id milestone-id)
  (concat (lighthouse-milestones-url (number-to-string project-id)) "/" (number-to-string milestone-id)))

(defun lighthouse-response (doc)
  (let ((json-object-type 'plist))
    (json-read-from-string doc)))

(defun lighthouse-retrieve-synchronously (url)
  (let ((furl-error-function 'lighthouse-handle-error))
    (lighthouse-response
     (furl-retrieve-synchronously
      (concat url ".json?_token=" lighthouse-api-token)))))

(defun lighthouse-handle-error (err info)
  "Handle a Lighthouse error by printing the response message."
  (let ((msg (cdr (assoc 'message (read (furl--get-response-body))))))
    (kill-buffer)
    (error (concat "Lighthouse error: " msg))))

(defun map-values (key plist)
  (mapcar (lambda (hash) (plist-get hash key)) plist))

(provide 'lighthouse-util)
