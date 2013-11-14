;; This buffer is for notes you don't want to save, and for Lisp evaluation.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.


(defgroup pastebin nil
  "Pastebin -- pastebin.com client"
  :tag "Pastebin"
  :group 'tools)

(defclass paste ()
  ((key :initarg :key)
   (date :initarg :date)
   (title :initarg :title)
   (size :initarg :size)
   (expire_date :initarg :expire_date)
   (private :initarg :private)
   (format_long :initarg :format_long)
   (format_short :initarg :format_short)
   (url :initarg :url)
   (buffer :initarg :buffer)
   (last-fetched :initarg :last-fetched)
   (hits :initarg :hits))
  "a paste")

(defclass paste-user ()
  ((dev-key :initarg :dev-key "Your developer key from http://pastebin.com/api")
   (usr-key :initarg :usr-key "Your user key, retrived from pastebin")
   (password :initarg :password "Your password, clear text honey!!")
   (username :initarg :username "Your username")))

(defcustom pastebin-post-request-login-url "http://pastebin.com/api/api_login.php"
  "Login url"
  :type 'string
  :group 'pastebin)

(defcustom pastebin-post-request-paste-url "http://pastebin.com/api/api_post.php"
  "Paste url"
  :type 'string
  :group 'pastebin)

(defun strip-CRs (&optional buffer)
  "Get rid of damn ^Ms"
  (let ((buffer (or buffer (current-buffer))))
    (with-current-buffer buffer
      (goto-char (point-min))
      (while (re-search-forward "\r\n" nil t)
        (replace-match ""))
      buffer)))

(defun strip-http-header (&optional buffer)
  "Given a buffer with an HTTP response, remove the header and return the buffer
If no buffer is given current buffer is used"
  (let ((buffer (or buffer (current-buffer))))
    (with-current-buffer buffer
      (goto-char (point-min))
      (re-search-forward "\n\n")
    (kill-region (point-min) (point))
    buffer)))

(defun paste-retrieve-contents-by-key-h (key)
  (unless (stringp key)
    (signal 'wrong-type-argument "`key' should be a string"))
  (with-current-buffer (url-retrieve-synchronously (concat "http://pastebin.com/raw.php?i=" key))
    (strip-http-header)))

(defun paste-retrieve-paste-list-xml-h (user &optional count)
  "Retrieve a list of count (100 by default) pastes from pastebin.com"
  (unless (paste-user-p user)
    (signal 'wrong-type-argument "`user' should be a `paste-user'"))
  (let* ((params (concat "api_dev_key=" (oref user dev-key)
                         "&api_user_key=" (oref user usr-key)
                         "&api_results_limits=" (or count (format "%s" 100))
                         "&api_option=" "list"))
         (url-request-method "POST")
         (url-request-extra-headers '(("Content-Type" . "application/x-www-form-urlencoded")))
         (url-request-data params))
    (with-current-buffer (url-retrieve-synchronously pastebin-post-request-paste-url)
      (strip-http-header)
      (strip-CRs)
      (current-buffer)
      )))

(setq me (paste-user "Me" :dev-key pastebin-unique-developer-api-key
                          :usr-key pastebin-user-api-key
                          :password pastebin-password
                          :username pastebin-user-name))




 (with-current-buffer (paste-retrieve-paste-list-xml-h me)
   (goto-char (point-min))
   (re-search-forward "</paste>")
   (xml-parse-region (point-min) (point)))



