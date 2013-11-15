(defgroup pastebin nil
  "Pastebin -- pastebin.com client"
  :tag "Pastebin"
  :group 'tools)

;; PASTE CLASS

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
   (user :initarg :user)
   (hits :initarg :hits)))

(defmethod assign-buffer ((p paste) buf)
  "Assing if not already assigned"
  (if (slot-boundp p :buffer)
      (oref p :buffer)
    (oset p :buffer buf)))

(defmethod paste-fetch ((p paste) &optional dont-set-buffer)
  "Fetch the raw content from paste and return buffer containing"
  (let* ((url-request-method "GET")
         (url-request-extra-headers '(("Content-Type" . "application/x-www-form-urlencoded"))))
    (with-current-buffer (url-retrieve-synchronously (concat pastebin-raw-paste-url (oref p key)))
      (strip-http-header)
      (unless dont-set-buffer
        (assign-buffer p (current-buffer)))
      (setq pastebin-local-buffer-paste p) ;; buffer local
      (current-buffer))))

;; TODO
(defmethod paste-delete ((p paste))
  "Detele paste from pastebin.com"
  (unless (and (slot-boundp p :user)
               (slot-boundp p :key)
               (slot-boundp (oref p :user) :dev-key) 
               (slot-boundp (oref p :user) :usr-key))
    (signal 'unbound-error "paste-delete called with ubound slot object"))

  (let* ((params (concat "api_dev_key=" (oref (oref p :user) :dev-key)
                         "&api_user_key=" (oref (oref p :user) :usr-key)
                         "&api_paste_key=" (oref p :key)
                         "&api_option=delete"))
         (url-request-method "POST")
         (url-request-extra-headers
          '(("Content-Type" . "application/x-www-form-urlencoded")))
         (url-request-data params))
    (with-current-buffer (url-retrieve-synchronously pastebin-post-request-paste-url)
      (strip-http-header)
      (buffer-string))
    ))



;; PASTE-USER class

(defclass paste-user ()
  ((dev-key :initarg :dev-key "Your developer key from http://pastebin.com/api")
   (usr-key :initarg :usr-key "Your user key, retrived from pastebin")
   (password :initarg :password "Your password, clear text honey!!")
   (username :initarg :username "Your username")
   (paste-list :initarg :paste-list "The list of pastes for this user")
   (buffer-hash :initarg :list-buffer "Done by do-list-buffer")
  ))

(defmethod is-logged ((user paste-user))
  (slot-boundp user :usr-key))

(defmethod fetch-list-xml ((user paste-user) &optional count)
  "Fetch the list of pastes as xml, and return that buffer"
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
      (current-buffer)
      )))

(defmethod refresh-paste-list ((user paste-user) &optional count)
  "Set/Refresh paste-list attr to the list of paste objects retrieved from pastebin.com"
  (oset user :paste-list nil)
  (with-current-buffer (fetch-list-xml user (or count pastebin-default-paste-list-limit))
    (goto-char (point-min))
    (let ((i (point-min)))
      (while (re-search-forward "</paste>" nil t)
        (let ((paste-sexp (xml-parse-region i (point))))
          (setq i (point))
          (push (paste (concat "paste@" (pastebin-paste-sexp-get-attr-h paste-sexp 'paste_key))
                               :key (pastebin-paste-sexp-get-attr-h paste-sexp 'paste_key)
                               :date (pastebin-paste-sexp-get-attr-h paste-sexp 'paste_date)
                               :title (pastebin-paste-sexp-get-attr-h paste-sexp 'paste_title)
                               :size (pastebin-paste-sexp-get-attr-h paste-sexp 'paste_size)
                               :expire_date (pastebin-paste-sexp-get-attr-h paste-sexp 'paste_expire_date)
                               :private (pastebin-paste-sexp-get-attr-h paste-sexp 'paste_private)
                               :format_long (pastebin-paste-sexp-get-attr-h paste-sexp 'paste_format_long)
                               :format_short (pastebin-paste-sexp-get-attr-h paste-sexp 'paste_format_short)
                               :url (pastebin-paste-sexp-get-attr-h paste-sexp 'paste_url)
                               :last-fetched (pastebin-paste-sexp-get-attr-h paste-sexp 'paste_last-fetched)
                               :user user) (oref user :paste-list))
          )
        )
      )
    )
  )

(defmethod do-list-buffer ((user paste-user))
  "Create a buffer with a list of pastes and return it
Some keybinds are setted"
  (interactive)

  (unless (is-logged user)
    (signal 'unlogged-user "do-list-buffer called with not logged user"))

  (oset user :list-buffer 
        (get-buffer-create (format "*Pastebin %s List*" (oref user :username))))
  
  ;; fetch pastes list
  (refresh-paste-list user)

  (with-current-buffer (get-buffer (oref user :list-buffer))

    (let ((inhibit-read-only t))
      (erase-buffer))

    (widget-minor-mode 1)
    (use-local-map pastebin-list-map)
    
    (setq pastebin-list-buffer-user user)

    (widget-insert (format "%5.5s | %-8.8s | %-32.32s | %-7.7s | %-30.30s\n"
                           "VIEW" "ID" "TITLE" "SYNTAX" "DATE"))
    (dolist (paste (oref user :paste-list))
      (widget-create 'link 
                     :notify (lambda (wid &rest ignore)
                               (pastebin-fetch-paste-at-point))
                     :paste paste
                     :follow-link t
                     :value (format "%4.4s | %-8.8s | %-32.32s | %-7.7s | %-20.20s"
                                    (if (string= (oref paste :private) "1")
                                        "PRIV"
                                      "PUBL")
                                    (oref paste :key)
                                    (or (oref paste :title) "")
                                    (oref paste :format_short)
                                    (format-time-string "%c" (seconds-to-time (string-to-number (oref paste :date))))
                                    )
                     )

      (widget-insert "\n")
      )
    (widget-setup)
    (goto-char (point-min))
    (current-buffer)
    )
  )

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
        (oset user :usr-key (buffer-substring-no-properties (point-min) (point-max)))))))




;; TODO
(defmethod paste-new ((user paste-user) &optional &key title syntax private buffer)
  "Upload a new paste to pastebin.com
Retrieve the paste object from pastebin
Refresh paste-list for user
Set the pastebin-minor-mode on current buffer"
  (let* ((ptitle (or title (buffer-name)))
         (pbuffer (or buffer (current-buffer)))
         (psyntax (or syntax "text"))
         (pprivate (or private "0"))
         (params (concat "api_dev_key=" (oref user :dev-key)
                         "&api_user_key=" (oref user :usr-key)
                         "&api_paste_name=" ptitle
                         "&api_paste_code=" (url-hexify-string (with-current-buffer pbuffer
                                                                 (buffer-string)))
                         "&api_option=paste"
                         "&api_paste_private=" pprivate))
         (url-request-method "POST")
         (url-request-extra-headers
          '(("Content-Type" . "application/x-www-form-urlencoded")))
         (url-request-data params))
    (with-current-buffer (url-retrieve-synchronously pastebin-post-request-paste-url)
      (strip-http-header)
      (buffer-string)))
  )

(defmethod fetch-and-process ((p paste))
  "Fetch buffer a do needed processing before switching to it"
  (with-current-buffer (paste-fetch p)
    (switch-to-buffer (current-buffer))))
    







;; Local variables and customs

(defcustom pastebin-type-assoc
  '((actionscript-mode . " actionscript")
    (ada-mode . "ada")
    (asm-mode . "asm")
    (autoconf-mode . "bash")
    (bibtex-mode . "bibtex")
    (cmake-mode . "cmake")
    (c-mode . "c")
    (c++-mode . "cpp")
    (cobol-mode . "cobol")
    (conf-colon-mode . "properties")
    (conf-javaprop-mode . "properties")
    (conf-mode . "ini")
    (conf-space-mode . "properties")
    (conf-unix-mode . "ini")
    (conf-windows-mode . "ini")
    (cperl-mode . "perl")
    (csharp-mode . "csharp")
    (css-mode . "css")
    (delphi-mode . "delphi")
    (diff-mode . "dff")
    (ebuild-mode . "bash")
    (eiffel-mode . "eiffel")
    (emacs-lisp-mode . "lisp")
    (erlang-mode . "erlang")
    (erlang-shell-mode . "erlang")
    (espresso-mode . "javascript")
    (fortran-mode . "fortran")
    (glsl-mode . "glsl")
    (gnuplot-mode . "gnuplot")
    (graphviz-dot-mode . "dot")
    (haskell-mode . "haskell")
    (html-mode . "html4strict")
    (idl-mode . "idl")
    (inferior-haskell-mode . "haskell")
    (inferior-octave-mode . "octave")
    (inferior-python-mode . "python")
    (inferior-ruby-mode . "ruby")
    (java-mode . "java")
    (js2-mode . "javascript")
    (jython-mode . "python")
    (latex-mode . "latex")
    (lisp-mode . "lisp")
    (lua-mode . "lua")
    (makefile-mode . "make")
    (makefile-automake-mode . "make")
    (makefile-gmake-mode . "make")
    (makefile-makepp-mode . "make")
    (makefile-bsdmake-mode . "make")
    (makefile-imake-mode . "make")
    (matlab-mode . "matlab")
    (nxml-mode . "xml")
    (oberon-mode . "oberon2")
    (objc-mode . "objc")
    (ocaml-mode . "ocaml")
    (octave-mode . "matlab")
    (pascal-mode . "pascal")
    (perl-mode . "perl")
    (php-mode . "php")
    (plsql-mode . "plsql")
    (po-mode . "gettext")
    (prolog-mode . "prolog")
    (python-2-mode . "python")
    (python-3-mode . "python")
    (python-basic-mode . "python")
    (python-mode . "python")
    (ruby-mode . "ruby")
    (scheme-mode . "lisp")
    (shell-mode . "bash")
    (sh-mode . "bash")
    (smalltalk-mode . "smalltalk")
    (sql-mode . "sql")
    (tcl-mode . "tcl")
    (visual-basic-mode . "vb")
    (xml-mode . "xml")
    (yaml-mode . "properties")
    (text-mode . "text"))
  "Alist composed of major-mode names and corresponding pastebin highlight formats."
  :type '(alist :key-type symbol :value-tupe string)
  :group 'pastebin)

(defvar pastebin-default-paste-list-limit 100
  "The number of pastes to retrieve by default")

(defvar pastebin-default-user nil
  "The default user begin used")

(defvar pastebin-local-buffer-paste nil
  "Every pastebin buffer has a paste object associated with it")
(make-variable-buffer-local 'pastebin-local-buffer-paste)

(defvar pastebin-list-buffer-user nil
  "Every pastebin list buffer has a user object associated with it")
(make-variable-buffer-local 'pastebin-list-buffer-user)

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

(defvar pastebin-list-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "d") 'pastebin-delete-paste-at-point) ;; TODO 
    (define-key map (kbd "r") 'pastebin-list-buffer-refresh) ;; TODO refresh pastebin list buffer
    map)
  "Key map for pastebin list buffer")

;; Local functions and helpers

(defun pastebin-paste-sexp-get-attr-h (paste-sexp attr)
  "Return the attribute `attr' from `paste'
Attributes are described here: http://pastebin.com/api#9
`attr' is a symbol
Ex: (pastebin-paste-get-attr (pastebin-pastes-nth 0) 'paste_tittle)"
  (unless (symbolp attr)
    (signal 'wrong-type-argument ("`attr' should be a symbol")))
  (car (last (assoc attr (nthcdr 2 (car paste-sexp))))))

(cl-defun pastebin-do-login (&key username password dev-key)
  "Interface layer, do the login and set `pastebin-default-user'"
  (unless (and username password dev-key)
    (error "pastebin-login argument missing"))
  
  (setq pastebin-default-user (paste-user username :username username
                                          :password password
                                          :dev-key dev-key))
  (login pastebin-default-user))

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

(defun pastebin-get-paste-at-point ()
  "Get the paste at point at current-buffer"
  (let ((wid (widget-at)))
    (if (not wid)
        (error "No paste at point")
      (widget-get wid :paste))))

(defun pastebin-fetch-paste-at-point ()
  "Fetch and switch to paste at point"
  (let ((paste (pastebin-get-paste-at-point)))
    (fetch-and-process paste)))

;; User interface 


(defun pastebin-list-buffer-refresh ()
  "Refresh the list buffer screen
Operates on current buffer"
  (interactive)
  (switch-to-buffer (do-list-buffer pastebin-default-user)))
  
;; FIXME, pastebin-list-buffer-refresh cracks here
(defun pastebin-delete-paste-at-point ()
  "Delete the paste at point"
  (interactive)
  (let ((paste (pastebin-get-paste-at-point)))
    (message "%s" (paste-delete paste))
    (pastebin-list-buffer-refresh)))

(defun pastebin-new ()
  "Create a new paste from buffer"
  (interactive)
  (message "URL %s" (paste-new pastebin-default-user)))

(pastebin-do-login :dev-key pastebin-unique-developer-api-key
                   :password pastebin-password
                   :username pastebin-user-name)


(provide 'pastebin-2.0)



;; (refresh-paste-list pastebin-default-user)
;; (setq paste-sexp '((paste nil (paste_key nil A96vA23q) (paste_date nil 1384544651) (paste_title nil *scratch*) (paste_size nil 191) (paste_expire_date nil 0) (paste_private nil 1) (paste_format_long nil None) (paste_format_short nil text) (paste_url nil http://pastebin.com/A96vA23q) (paste_hits nil 0))))
