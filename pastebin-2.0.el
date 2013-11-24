;;; pastebin.el --- pastebin.com interface to emacs

;;; Copyright (C) 2013 by Daniel Hilst <danielhilst at gmail.com>

;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.

;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with this program; see the file COPYING.  If not, write to the
;;; Free Software Foundation, Inc.,   51 Franklin Street, Fifth Floor,
;;; Boston, MA  02110-1301  USA

;;; Besides being a new interface, some parts were borrowed from old
;;; interface so I think is fair put the names here. 
;;; Copyright (C) 2008 by Nic Ferrier <nic@ferrier.me.uk>
;;; Copyright (C) 2010 by Ivan Korotkov <twee@tweedle-dee.org>
;;; Copyright (C) 2012 by Filonenko Michael <filonenko.mikhail@gmail.com>

;;;
;;; Naming convention:
;;;
;;; pastebin-- prefix for internal stuff
;;; pastebin- prefix for user interface and customs
;;;
;;;
;;; @TODO list:
;;;
;;; - On pasting a new paste:
;;;   - Set syntax based on mode
;;;   - Set privacy depending o command prefix
;;;   - Setting pastebin minor mode on the buffer pasted
;;;   - Check if paste was not block by containing an url
;;;
;;; - On fetching the paste
;;;   - Set the mode based on paste's syntax
;;;   - Set pastebin-minor-mode on it
;;;  
;;; - pastebin minor mode
;;;   - Must save files on pastebin with keybinds. I'm
;;;     wondering if setting pastebin.com as an abstract storage
;;;     is a good idea. If so, C-x C-s should save pastebin buffers
;;;     to pastebin.com without question. Elisp files manual chapter would help-me
;;;     to do that http://www.gnu.org/software/emacs/manual/html_node/elisp/Files.html#Files
;;;
;;; - error checking
;;;   - Currently, no error from pastebin or http error is checked. I should
;;;     check all they!
;;;   - Here is another error while pastebin-new "URL Post limit, maximum pastes per 24h reached"
;;;
;;; DEPENDENCIES
;;;
;;; eieio.el
;;;   

(require 'eieio)

(defgroup pastebin nil
  "Pastebin -- pastebin.com client"
  :tag "Pastebin"
  :group 'tools)

;; Customs 

(defcustom pastebin-default-paste-list-limit 100
  "The number of pastes to retrieve by default"
  :type 'number
  :group 'pastebin)

(defcustom pastebin-post-request-login-url "http://pastebin.com/api/api_login.php"
  "Login url"
  :type 'string
  :group 'pastebin)

(defcustom pastebin-post-request-paste-url "http://pastebin.com/api/api_post.php"
  "Paste url"
  :type 'string
  :group 'pastebin)

(defcustom pastebin-data-dir "~/.emacs.d/pastebin-data"
  "Diretory to keep data"
  :type 'string
  :group 'pastebin)

;; Global variables

(defvar pastebin--type-assoc
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
  "Alist composed of major-mode names and corresponding pastebin highlight formats.")

(defvar pastebin--default-user nil
  "The default user begin used")

(defvar pastebin--local-buffer-paste nil
  "Every pastebin buffer has a paste object associated with it")
(make-variable-buffer-local 'pastebin--local-buffer-paste)

(defvar pastebin--list-buffer-user nil
  "Every pastebin list buffer has a user object associated with it")
(make-variable-buffer-local 'pastebin--list-buffer-user)

(defvar pastebin--list-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "d") 'pastebin-delete-paste-at-point) ;; TODO 
    (define-key map (kbd "r") 'pastebin-list-buffer-refresh) ;; TODO refresh pastebin list buffer
    (define-key map (kbd "f") 'pastebin-list-buffer-refresh-sort-by-format)
    (define-key map (kbd "t") 'pastebin-list-buffer-refresh-sort-by-title)
    (define-key map (kbd "k") 'pastebin-list-buffer-refresh-sort-by-key)
    (define-key map (kbd "D") 'pastebin-list-buffer-refresh-sort-by-date)
    (define-key map (kbd "p") 'pastebin-list-buffer-refresh-sort-by-private)
    map)
  "Key map for pastebin list buffer")

(defconst pastebin--raw-paste-url "http://pastebin.com/raw.php?i="
  "Concatenate this with paste key to get the raw paste")

;;
;; EIEIO Layer
;;

;; PASTE-USER class

(defclass pastebin--paste-user ()
  ((dev-key :initarg :dev-key "Your developer key from http://pastebin.com/api")
   (usr-key :initarg :usr-key "Your user key, retrived from pastebin")
   (password :initarg :password "Your password, clear text honey!!")
   (username :initarg :username "Your username")
   (paste-list :initarg :paste-list "The list of pastes for this user")
   (list-buffer :initarg :list-buffer "Done by do-list-buffer")
   (sort-by :initarg :sort-by "Order to sort :paste-list")
  )
  "Class representing a pastebin.com user")

(defmethod is-logged ((user pastebin--paste-user))
  "Return true if user is logged in"
  (slot-boundp user :usr-key))

(defmethod fetch-list-xml ((user pastebin--paste-user))
  "Fetch the list of pastes as xml, and return that buffer"
  (let* ((params (concat "api_dev_key=" (oref user dev-key)
                         "&api_user_key=" (oref user usr-key)
                         "&api_results_limits=" (format "%d" pastebin-default-paste-list-limit)
                         "&api_option=list")
                         )
         (url-request-method "POST")
         (url-request-extra-headers '(("Content-Type" . "application/x-www-form-urlencoded")))
         (url-request-data params))
    (with-current-buffer (url-retrieve-synchronously pastebin-post-request-paste-url)
      (pastebin--strip-http-header)
      (pastebin--strip-CRs)
      (goto-char (point-min))
      (current-buffer)
      )))

(defmethod refresh-paste-list ((user pastebin--paste-user))
  "Set/Refresh paste-list attr to the list of paste objects retrieved from pastebin.com"
  (oset user :paste-list nil)
  (with-current-buffer (fetch-list-xml user)
    (goto-char (point-min))
    (let ((i (point-min))
          plist)
      (while (re-search-forward "</paste>" nil t)
        (let ((paste-sexp (xml-parse-region i (point)))
              p)
          (setq i (point))
          (condition-case err
              (progn
                (setq p (pastebin--sexp-to-paste paste-sexp))
                (oset p :user user)
                (oset p :last-fetched (float-time))
                (setq plist (append plist (list p))))
            (wrong-type-argument
             (error "Error while creating paste object on `refresh-paste-list' %s" err)
             (debug)))
          )
        (oset user :paste-list plist)
        )
      )
    )
  )

(defmacro pastebin--sort-by-string-attr (user attr)
  "sort :paste-list by `attr' in reverse order"
  `(progn
     (unless (keywordp ,attr)
       (error "pastebin--sort-by-stirng-attr attr is not a keyword"))

     (unless (member ,attr '(:key :title :format_long :format_short :url :date :private))
      (error "pastebin--sort-by-string-attr attr is not in '(:key :title :format_long :format_short :url)"))

     (oset ,user :paste-list (sort (oref ,user :paste-list) (lambda (p1 p2)
                                                          (string< (downcase (oref p1 ,attr))
                                                                   (downcase (oref p2 ,attr))))))
     )
  )

(defmethod do-list-buffer ((user pastebin--paste-user))
  "Create a buffer with a list of pastes and return it
Some keybinds are setted"
  (unless (is-logged user)
    (error "do-list-buffer called with unloged user"))

  (unless (slot-boundp user :list-buffer)
    (oset user :list-buffer (format "Pastebin %s pastes" (oref user :username))))

  (let ((inhibit-read-only t)
        old-point (point))
    (with-current-buffer (get-buffer-create (oref user :list-buffer))

      (erase-buffer)

      (widget-minor-mode 1)
      (use-local-map pastebin--list-map)
      
      (setq pastebin--list-buffer-user user)

      (widget-insert (format "%5.5s | %-8.8s | %-32.32s | %-7.7s | %-30.30s\n"
                             "VIEW" "ID" "TITLE" "FORMAT" "DATE"))
      (dolist (paste (oref user :paste-list))
        (widget-create 'link 
                       :notify (lambda (wid &rest ignore)
                                 (pastebin--fetch-paste-at-point))
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
      (goto-char (or old-point (point-min)))
      (current-buffer)
      ) ;; (with-current-buffer (get-buffer (oref user :list-buffer))
    ) ;; (let ((inhibit-read-only t)
  )

;; @TODO Error checking for bad user/passwor/dev-key here
(defmethod login ((user pastebin--paste-user))
  "Given user and password login and sets usr-key"
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
        (pastebin--strip-http-header)
        (oset user :usr-key (buffer-substring-no-properties (point-min) (point-max)))))))

;; @TODO: If I paste something containing an url, pastebin
;; blocks the paste until I pass a captha. I need to check
;; if paste was really pasted and if not tell the user that
;; he needs pass the captcha
(defmethod paste-new ((user pastebin--paste-user) &optional &key title syntax private buffer)
  "Upload a new paste to pastebin.com"
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
      (pastebin--strip-http-header)
      (buffer-string)))
  )



;; PASTE CLASS

(defclass pastebin--paste ()
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
   (user :initarg :user :type pastebin--paste-user)
   (hits :initarg :hits))
  "Class representing a paste from pastebin.com 
The contents of paste are not stored. Instead the method
`paste-fetch' fetch and retrieve the buffer with paste contents")

(defmethod get-mode ((p pastebin--paste))
  "return the mode from `pastebin--type-assoc'"
  (if (slot-boundp p :format_short)
      (car (rassoc (oref p :format_short) pastebin--type-assoc))
    (error "No format short for paste %s with key %s" (oref p :title) (oref p :key))))

(defmethod fetch-and-process ((p pastebin--paste))
  "Fetch buffer a do needed processing before switching to it"
  (with-current-buffer (paste-fetch p)
    (switch-to-buffer (current-buffer))))
    
(defmethod paste-fetch ((p pastebin--paste))
  "Fetch the raw content from paste and return buffer containing"
  (let* ((url-request-method "GET")
         (url-request-extra-headers '(("Content-Type" . "application/x-www-form-urlencoded")))
         (content-buf (url-retrieve-synchronously (concat pastebin--raw-paste-url (oref p key))))
         (inhibit-read-only t)
         (pbuf (if (slot-boundp p :buffer)
                   (oref p :buffer)
                 (get-buffer-create (concat "*paste " (oref p :title ) "*")))))
    (pastebin--strip-http-header content-buf)
    (oset p :buffer pbuf)
    (with-current-buffer pbuf
      (erase-buffer)
      (insert-buffer content-buf)
      (funcall (get-mode p))
      (setq pastebin--local-buffer-paste p) ;; buffer local
      (current-buffer))))

(defmethod paste-delete ((p pastebin--paste))
  "Detele paste from pastebin.com"
  (unless (and (slot-boundp p :user)
               (slot-boundp p :key)
               (slot-boundp (oref p :user) :dev-key) 
               (slot-boundp (oref p :user) :usr-key))
    (error "paste-delete called with ubound slot object"))

  (let* ((params (concat "api_dev_key=" (oref (oref p :user) :dev-key)
                         "&api_user_key=" (oref (oref p :user) :usr-key)
                         "&api_paste_key=" (oref p :key)
                         "&api_option=delete"))
         (url-request-method "POST")
         (url-request-extra-headers
          '(("Content-Type" . "application/x-www-form-urlencoded")))
         (url-request-data params))
    (with-current-buffer (url-retrieve-synchronously pastebin-post-request-paste-url)
      (pastebin--strip-http-header)
      (buffer-string)) ;; Pastebin send somthing like paste xxx deleted
    ))

;; Local functions and helpers

(defun pastebin--sexp-get-attr-h (paste-sexp attr)
  "Return the attribute `attr' from `paste-sexp'
Attributes are described here: http://pastebin.com/api#9
`attr' must be a symbol
Ex: (pastebin-paste-get-attr some-paste-sexp 'paste_tittle)"
  (unless (symbolp attr)
    (error "attr should be a symbol"))
  (let ((a (car (last (assoc attr (nthcdr 2 (car paste-sexp)))))))
    (unless a
      (error "No attribute %s on this paste-sexp" attr))
    (format "%s" a)))

(defun pastebin--strip-CRs (&optional buffer)
  "Get rid of CRLF
I need this for xml-parse-region reponse without getting
a lot of spaces and CRLF on pastes sexps. See `pastebin--sexp-to-paste'"
  (let ((buffer (or buffer (current-buffer))))
    (with-current-buffer buffer
      (goto-char (point-min))
      (while (re-search-forward "\r\n" nil t)
        (replace-match ""))
      buffer)))

(defun pastebin--strip-http-header (&optional buffer)
  "Given a buffer with an HTTP response, remove the header and return the buffer
If no buffer is given current buffer is used"
  (let ((buffer (or buffer (current-buffer))))
    (with-current-buffer buffer
      (goto-char (point-min))
      (re-search-forward "\n\n")
    (kill-region (point-min) (point))
    buffer)))

(defun pastebin--get-paste-at-point ()
  "Get the paste at point at current-buffer"
  (let ((wid (widget-at)))
    (if (not wid)
        (error "No paste at point")
      (widget-get wid :paste))))

(defun pastebin--fetch-paste-at-point ()
  "Fetch and switch to paste at point"
  (let ((p (pastebin--get-paste-at-point)))
    (fetch-and-process p)))

(defun pastebin--sexp-to-paste (paste-sexp)
  "Given and sexp returned from `xml-parse-region' on pastebin.com response, constructs and return a pastebin--paste object.
See `fetch-list-xml' for more information"
  (unless (consp paste-sexp)
    (error "pastebin--sexp-to-paste called without cons type"))
  (condition-case err
      (pastebin--paste (concat "paste@" (pastebin--sexp-get-attr-h paste-sexp 'paste_key))
             :key (pastebin--sexp-get-attr-h paste-sexp 'paste_key)
             :date (pastebin--sexp-get-attr-h paste-sexp 'paste_date)
             :title (pastebin--sexp-get-attr-h paste-sexp 'paste_title)
             :size (pastebin--sexp-get-attr-h paste-sexp 'paste_size)
             :expire_date (pastebin--sexp-get-attr-h paste-sexp 'paste_expire_date)
             :private (pastebin--sexp-get-attr-h paste-sexp 'paste_private)
             :format_long (pastebin--sexp-get-attr-h paste-sexp 'paste_format_long)
             :format_short (pastebin--sexp-get-attr-h paste-sexp 'paste_format_short)
             :url (pastebin--sexp-get-attr-h paste-sexp 'paste_url)
             )
    ((debug error)
     (error "Cant construct paste from sexp %s\nError: %s" paste-sexp err))))

(defun pastebin--store-password (passwd)
  "Stores password on `pastebin-data-dir'/pass"
  (pastebin--mkdatadir)
  (with-temp-buffer
    (insert passwd)
    (write-file (concat pastebin-data-dir "/pass"))))

(defun pastebin--read-password-from-file ()
  "Read password from `pastebin-data-dir'/pass"
  (with-temp-buffer
    (goto-char (point-min))
    (insert-file-literally (concat pastebin-data-dir "/pass"))
    (buffer-string)))

(defun pastebin--password-file-exists-p ()
  "return t if pastebin-data-dir exists"
  (file-exists-p (concat pastebin-data-dir "/pass")))

(defun pastebin--mkdatadir ()
  "Create the `pastebin-data-dir'"
  (ignore-errors
    (make-directory pastebin-data-dir t)))

(defun pastebin--ask-for-password (prompt)
  (lexical-let ((p (read-passwd prompt)))
    (when (yes-or-no-p "Store password on disk? ")
        (pastebin--store-password p))
    p))

;; User interface 

(defun pastebin-list-buffer-refresh ()
  "Refresh the list buffer screen
Operates on current buffer"
  (interactive)
  (refresh-paste-list pastebin--default-user)
  (switch-to-buffer (do-list-buffer pastebin--default-user))
  )


(defun pastebin-list-buffer-refresh-sort-by-title ()
  (interactive)
  (pastebin--sort-by-string-attr pastebin--default-user :title)
  (switch-to-buffer (do-list-buffer pastebin--default-user))
  )

(defun pastebin-list-buffer-refresh-sort-by-format ()
  (interactive)
  (pastebin--sort-by-string-attr pastebin--default-user :format_short)
  (switch-to-buffer (do-list-buffer pastebin--default-user))
  )

(defun pastebin-list-buffer-refresh-sort-by-key ()
  (interactive)
  (pastebin--sort-by-string-attr pastebin--default-user :key)
  (switch-to-buffer (do-list-buffer pastebin--default-user))
  )

(defun pastebin-list-buffer-refresh-sort-by-date ()
  (interactive)
  (pastebin--sort-by-string-attr pastebin--default-user :date)
  (oset pastebin--default-user :paste-list (reverse (oref pastebin--default-user :paste-list)))
  (switch-to-buffer (do-list-buffer pastebin--default-user))
  )

(defun pastebin-list-buffer-refresh-sort-by-private ()
  (interactive)
  (pastebin--sort-by-string-attr pastebin--default-user :private)
  (switch-to-buffer (do-list-buffer pastebin--default-user))
  )

(defun pastebin-delete-paste-at-point ()
  "Delete the paste at point"
  (interactive)
  (lexical-let ((p (pastebin--get-paste-at-point)))
    (when (y-or-n-p (format "Do you really want to delete paste %s from %s\n" 
                            (oref p :title)
                            (format-time-string "%c" (seconds-to-time (string-to-number (oref p :date))))))
      (message "%s" (paste-delete (pastebin--get-paste-at-point))))
    (pastebin-list-buffer-refresh)))

(defun pastebin-new ()
  "Create a new paste from buffer"
  (interactive)
  (message "URL %s" (paste-new pastebin--default-user)))

(defun* pastebin-do-login (&key username dev-key password)
  "Interface layer, do the login and set `pastebin--default-user'"
  (unless (and username dev-key)
    (error "pastebin-login argument missing. (dev-key or username)"))
  (lexical-let ((p (if (pastebin--password-file-exists-p)
                       (pastebin--read-password-from-file)
                     (pastebin--ask-for-password "Pastebin password: "))))
    (setq pastebin--default-user (pastebin--paste-user username
                                                     :username username
                                                     :dev-key  dev-key
                                                     :password p)))
  (login pastebin--default-user)
  (message "User %s logged on pastebin.com! Have a nice day!" username))

;; @TODO: REMOVE THIS!
(pastebin-do-login :dev-key pastebin-unique-developer-api-key
                   :username pastebin-user-name)




(provide 'pastebin-2.0)

