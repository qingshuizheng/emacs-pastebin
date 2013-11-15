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
   (username :initarg :username "Your username")
   (paste-list :initarg :paste-list "The list of pastes for this user")
  ))

(defmethod login ((user paste-user))
  "Given user and password login and sets dev-key"
  (if (slot-boundp user :usr-key)
      (oref user :usr-key)
    (let* ((params (concat "api_dev_key=" (oref user :dev-key)
                           "&api_user_name=" (url-hexify-string (oref user :username))
                           "&api_user_password=" (url-hexify-string (oref user :password))))
           (url-request-method "POST")
           (url-request-extra-headers
            '(("Content-Type" . "application/x-www-form-urlencoded")))
           (url-request-data params))
      (with-current-buffer (url-retrieve-synchronously pastebin-post-request-login-url)
        (strip-http-header)
        (oset user :usr-key (buffer-substring-no-properties (point-min) (point-max)))
        (switch-to-buffer (current-buffer))))))

(defmethod assign-buffer ((p paste) buf)
  (if (slot-boundp p :buffer)
      (oref p :buffer)
    (oset p :buffer buf)))

(defmethod fetch ((p paste) &optional dont-set-buffer)
  "Fetch the raw content from paste and return buffer containing"
  (let* ((url-request-method "POST")
         (url-request-extra-headers '(("Content-Type" . "application/x-www-form-urlencoded")))
         (url-request-data params))
    (with-current-buffer (url-retrieve (concat pastebin-raw-paste-url (oref p key)))
      (strip-http-header)
      (unless dont-set-buffer
        (assign-buffer p (current-buffer)))
      (current-buffer))))

(defmethod fetch-list-xml ((user paste-user) &optional count)
  (let* ((params (concat "api_dev_key=" (oref user dev-key)
                         "&api_user_key=" (oref user usr-key)
                         "&api_results_limits=" (format "%d" (or count 100))
                         "&api_option=" "list"))
         (url-request-method "POST")
         (url-request-extra-headers '(("Content-Type" . "application/x-www-form-urlencoded")))
         (url-request-data params))
    (with-current-buffer (url-retrieve-synchronously pastebin-post-request-paste-url)
      (strip-http-header)
      (strip-CRs)
      (goto-char (point-max))
      (insert params)
      (current-buffer)
      )))

(setq me (paste-user "Me" :dev-key pastebin-unique-developer-api-key
                          :password pastebin-password
                          :username pastebin-user-name))

(defmethod set-paste-list ((user paste-user) &optional count)
  "Set paste-list attr to the list of paste objects retrieved from pastebin.com"
  (with-current-buffer (fetch-list-xml me count)
    (goto-char (point-min))
    (or (let ((i (point-min))
              plist)
          (while (re-search-forward "</paste>" nil t)
            (let ((p (xml-parse-region i (point))))
              (setq plist (cons (paste (concat "paste@" (pastebin-paste-sexp-get-attr-h p 'paste_key))
                                       :key (pastebin-paste-sexp-get-attr-h p 'paste_key)
                                       :title (pastebin-paste-sexp-get-attr-h p 'paste_title)
                                       :date (pastebin-paste-sexp-get-attr-h p 'paste_date)
                                       :size (pastebin-paste-sexp-get-attr-h p 'paste_size)
                                       :expire_date (pastebin-paste-sexp-get-attr-h p 'paste_expire_date)
                                       :private (pastebin-paste-sexp-get-attr-h p 'paste_private)
                                       :format_long (pastebin-paste-sexp-get-attr-h p 'paste_format_long)
                                       :url (pastebin-paste-sexp-get-attr-h p 'paste_url)
                                       :buffer (current-buffer)
                                       :last-fetched (float-time)
                                       :hits (pastebin-paste-sexp-get-attr-h p 'paste_hits)) plist))
              (setq i (point))))
          (oset user :paste-list plist))
        (error "Cant parse any paste"))))

(defun pastebin-paste-sexp-get-attr-h (paste-sexp attr)
  "Return the attribute `attr' from `paste'
Attributes are described here: http://pastebin.com/api#9
`attr' is a symbol
Ex: (pastebin-paste-get-attr (pastebin-pastes-nth 0) 'paste_tittle)"
  (unless (symbolp attr)
    (signal 'wrong-type-argument ("`attr' should be a symbol")))
  (car (last (assoc attr (nthcdr 2 (car paste-sexp))))))

(defconst pastebin-raw-paste-url "http://pastebin.com/raw.php?i="
  "Concatenate this with paste key to get the raw paste")

(defcustom pastebin-post-request-login-url "http://pastebin.com/api/api_login.php"
  "Login url"
  :type 'string
  :group 'pastebin)

(defcustom pastebin-post-request-paste-url "http://pastebin.com/api/api_post.php"
  "Paste url"
  :type 'string
  :group 'pastebin)

(defcustom pastebin-pastes-list nil
  "The list of `paste' instances"
  :type 'cons
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







