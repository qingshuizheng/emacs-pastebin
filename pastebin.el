;;; pastebin.el --- A simple interface to the www.pastebin.com webservice

;;; Copyright (C) 2008 by Nic Ferrier <nic@ferrier.me.uk>
;;; Copyright (C) 2010 by Ivan Korotkov <twee@tweedle-dee.org>
;;; Copyright (C) 2012 by Filonenko Michael <filonenko.mikhail@gmail.com>
;;; Copyright (C) 2012 by Daniel Hilst <danielhilst@gmail.com>

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

;;; Commentary:
;;;
;;; Make pastebin account:
;;;
;;;   http://pastebin.com/signup
;;; 
;;; Get Unique developer key:
;;;
;;;   http://pastebin.com/api  
;;;
;;; Setup emacs:
;;;
;;;   (require 'pastebin)
;;;   (setq pastebin-unique-developer-api-key "HEX") ;; from http://pastebin.com/api
;;;   (setq pastebin-user-name "username")
;;;   (setq pastebin-password "password")
;;;
;;;
;;; To send the whole buffer or select region and run
;;;
;;;  M-x pastebin
;;;
;;; List pastes
;;;
;;;   M-x pastebin-list-buffer
;;;
;;;   On pastebin list buffer you have this binds
;;;
;;;   `d' Delete the paste at point
;;;   `r' Reload the pastes list
;;;   RET Open the paste on new buffer
;;;
;;; To replace a paste do the above. The paste will deleted
;;; and posted again, this will change its key, and so the url.
;;;
;;;   M-x pastebin-replace
;;;
;;; The login will happen at first call to pastebin, pastebin-list-buffer
;;; or pastebin-replace
;;;
;;; In either case the url that pastebin generates is left on the kill
;;; ring and the paste buffer.


;;; Code:

(defgroup pastebin nil
  "Pastebin -- pastebin.com client"
  :tag "Pastebin"
  :group 'tools)

(defcustom pastebin-unique-developer-api-key "INSERT_YOURS"
  "Everybody using our API is required to use a valid Developer
 API Key. You automaticly get a key when you become a member of
 Pastebin. http://pastebin.com/api"
  :type 'string
  :group 'pastebin)

(defcustom pastebin-user-api-key nil
  "This is your login key, the function pastebin-login sets this."
  :type 'string
  :group 'pastebin)

(defcustom pastebin-user-name "guest"
  "Username of the user you want to login."
  :type 'string
  :group 'pastebin)

(defcustom pastebin-password "guest"
  "Password of the user you want to login."
  :type 'string
  :group 'pastebin)

(defcustom pastebin-post-request-login-url "http://pastebin.com/api/api_login.php"
  "Login url"
  :type 'string
  :group 'pastebin)

(defcustom pastebin-post-request-paste-url "http://pastebin.com/api/api_post.php"
  "Paste url"
  :type 'string
  :group 'pastebin)

(defun pastebin-delete-paste-at-point ()
  "Delete a paste at point, in `pastebin-list-buffer' buffer"
  (interactive)
  (pastebin-login-if-not-already)
  (let ((wid (widget-at)))
    (if (not wid)
        (error "No paste at point")
      (let ((oldpoint (point)))
        (pastebin-paste-delete (widget-get wid :paste))
        (pastebin-pastes-list-fetch)
        (while (not pastebin-pastes-list)
          (sleep-for 0.2))

        (pastebin-list-buffer)
        (goto-char oldpoint)
        )
      )
    )
  )

(defun pastebin-login-if-not-already ()
  "Log in if not already logged"
  (unless pastebin-user-api-key
    (pastebin-login-sync)))

(defun pastebin-login-sync ()
  "Do a synchronous login on pastebin, and set `pastebin-user-api-key'"
  (let* ((params (concat "api_dev_key=" pastebin-unique-developer-api-key
                         "&api_user_name=" (url-hexify-string pastebin-user-name)
                         "&api_user_password=" (url-hexify-string pastebin-password)))
         (url-request-method "POST")
         (url-request-extra-headers
          '(("Content-Type" . "application/x-www-form-urlencoded")))
         (url-request-data params))
    (with-current-buffer (url-retrieve-synchronously
                          pastebin-post-request-login-url)
      (strip-http-header)
      (setq pastebin-user-api-key (buffer-substring-no-properties (point) (point-max))))))

(defun strip-http-header (&optional buffer)
  "Given a buffer with an HTTP response, remove the header and return the buffer
If no buffer is given current buffer is used"
  (let ((buffer (or buffer (current-buffer))))
    (with-current-buffer buffer
      (goto-char (point-min))
      (re-search-forward "\n\n")
    (kill-region (point-min) (point))
    buffer)))

(defun pastebin-login ()
  "Asynchronous login
Uses `pastebin-unique-developer-api-key', `pastebin-user-name', and `pastebin-password'"
  (let* ((params (concat "api_dev_key=" pastebin-unique-developer-api-key
                         "&api_user_name=" (url-hexify-string pastebin-user-name)
                         "&api_user_password=" (url-hexify-string pastebin-password)))
         (url-request-method "POST")
         (url-request-extra-headers
          '(("Content-Type" . "application/x-www-form-urlencoded")))
         (url-request-data params)
         (content-buf (url-retrieve
                       pastebin-post-request-login-url
                       (lambda (arg)
                         (cond
                          ((equal :error (car arg))
                           (signal 'pastebin-error (cdr arg)))
                          (t
                           (re-search-forward "\n\n")
                           (setq pastebin-user-api-key (buffer-substring (point) (point-max)))))))))))


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

(defcustom pastebin-type-assoc
  '((never . "N") ;;    N = Never
    (ten-minutes . "10M")             ;; 10M = 10 Minutes
    (one-hour . "1H")  ;; 1H = 1 Hour
    (one-day . "1D")     ;; 1D = 1 Day
    (one-month . "1M"))     ;; 1M = 1 Month 
    "We have 5 valid values available which you can use with the 'api_paste_expire_date' parameter."
    :type '(alist :key-type symbol :value-tupe string)
    :group 'pastebin)

(defvar pastebin-domain-history '())

(defvar pastebin-map
  (let ((pastebin-map (make-sparse-keymap)))
    (define-key pastebin-map (kbd "d") 'pastebin-delete-paste-at-point)
    (define-key pastebin-map (kbd "r") 'pastebin-list-buffer)
    pastebin-map)
  "Key map for pastebin list buffer")

(defun pastebin (start end &optional name user-params)
  "Send the region to the pastebin service specified.

See pastebin.com for more information about pastebin.

Called interactively pastebin uses the current region for
preference for sending... if the mark is NOT set then the entire
buffer is sent.

Argument START is the start of region.
Argument END is the end of region.

If domain is used pastebin prompts for a domain defaulting to
'pastebin-default-domain' so you can send requests or use a
different domain.
"
  (interactive 
   (if (region-active-p)
       (list (region-beginning) (region-end) nil)
     (list (point-min) (point-max) nil)))
  ;; Main function
  (pastebin-login-if-not-already)
  (let* ((data (buffer-substring-no-properties start end))
         (params (concat "api_dev_key=" pastebin-unique-developer-api-key
                         "&api_user_key=" pastebin-user-api-key
                         "&api_option=" "paste"
                         "&api_paste_code=" (url-hexify-string data)
                         "&api_paste_format=" (or
                                               (assoc-default major-mode pastebin-type-assoc nil "text")
                                               "text")
                         (if name
                             (concat "&api_paste_name=" (url-hexify-string name))
                           (concat "&api_paste_name=" (url-hexify-string (buffer-name))))
                         
                         (when user-params
                           (concat "&" (url-hexify-string user-params)))
                         ))
         (url-request-method "POST")
         (url-request-extra-headers
          '(("Content-Type" . "application/x-www-form-urlencoded")))
         (url-request-data
          params)
         (content-buf (url-retrieve 
                       pastebin-post-request-paste-url
                       (lambda (arg)
                         (cond
                          ((equal :error (car arg))
                           (signal 'pastebin-error (cdr arg)))
                          (t
                           (re-search-forward "\n\n")
                           (clipboard-kill-ring-save (point) (point-max))
                           (message "Pastebin URL: %s" (buffer-substring (point) (point-max)))))))))))

(defun pastebin-pastes-list-fetch (&optional limit)
  "Fetches a list of pastes and save on `pastebin-pastes-list'"
  (setq pastebin-pastes-list nil)
  (let* ((params (concat "api_dev_key=" pastebin-unique-developer-api-key
                         "&api_user_key=" pastebin-user-api-key
                         "&api_results_limits=100"
                         "&api_option=" "list"))
         (url-request-method "POST")
         (url-request-extra-headers
          '(("Content-Type" . "application/x-www-form-urlencoded")))
         (url-request-data
          params)
         (content-buf (url-retrieve
                       pastebin-post-request-paste-url
                       (lambda (arg)
                         (cond
                          ((equal :error (car arg))
                           (signal 'pastebin-error (cdr arg)))
                          (t
                           (re-search-forward "\n\n")
                           (kill-region (point-min) (point))
                           (goto-char (point-min))
                           (while (re-search-forward "\r\n" nil t)
                             (replace-match ""))
                           (goto-char (point-min))
                           (insert "<root>")
                           (goto-char (point-max))
                           (insert "</root>")
                           (setq pastebin-pastes-list (xml-parse-region (point-min) (point-max)))))))))))

(defmacro pastebin-pastes ()
  "Return the list from `pastebin-pastes-list', without root and such"
  `(nthcdr 2 (car pastebin-pastes-list)))

(defmacro pastebin-pastes-foreach-paste (&rest body)
  "Iterates over `pastebin-pastes-list' with `paste' pointing to current paste"
  `(dolist (paste (pastebin-pastes))
     ,@body))

(defun pastebin-pastes-find-by-title (paste-title)
  "Given a `paste-title' return the paste from `pastebin-pastes-list'"
  (catch 'paste-found
    (dolist (paste (pastebin-pastes))
      (when (string= paste-title (pastebin-paste-get-attr paste 'paste_title))
        (message "found")
        (throw 'paste-found paste))))) ;; kind of break

;; @TODO
(defun pastebin-pastes-list-expired? ()
  "Return t if `pastebin-pastes-list' is considered too old"
  t)

(defvar pastebin-pastes-list nil)

;; FIXME: When I ran this, I loose the paste key
;; I need to keep paste's keys between replaces
;; >> I can't keep keys since Pastebin API don't 
;; support this.
;;
;; FIXME: Sometimes Pastebin anti-spam is triggered.
;; I need to refetch pastes list to see if my paste was
;; really replaced, if not I need to alert user to open
;; url retrieved from Pastebin and feed the captcha
(defun pastebin-replace (b e &optional name)
  "Replace a paste on pastebin.com based on current buffer"
  (interactive
   (if (region-active-p)
       (list (region-beginning) (region-end) nil)
     (list (point-min) (point-max) nil)))

  (pastebin-login-if-not-already)

  ;; Update list
  (when (pastebin-pastes-list-expired?)
      (pastebin-pastes-list-fetch))

  (while (not pastebin-pastes-list)
    (sleep-for 0.2))

  (let* ((paste-name (buffer-name))
        (paste-found (pastebin-pastes-find-by-title paste-name))
        (paste-key (when paste-found
                     (pastebin-paste-delete paste-found)
                     (concat "api_paste_key=" (pastebin-paste-get-attr paste-found 'paste_key)))))
    (pastebin b e nil paste-key))
  )

(defun pastebin-pastes-nth (nth)
  "Return `nth' paste from `pastebin-pastes-list'"
  (elt (pastebin-pastes) nth))

(defun pastebin-paste-get-attr (paste attr)
  "Return the attribute `attr' from `paste'
Attributes are described here: http://pastebin.com/api#9
`attr' is a symbol
Ex: (pastebin-paste-get-attr (pastebin-pastes-nth 0) 'paste_tittle)"
  (unless (symbolp attr)
    (signal 'wrong-type-argument ("`attr' should be a symbol")))
  (car (last (assoc attr paste))))

(defun pastebin-paste-delete (paste)
  "Delete paste from pastebin.com"
  (let* ((params (concat "api_dev_key=" pastebin-unique-developer-api-key
                         "&api_user_key=" pastebin-user-api-key
                         "&api_paste_key=" (pastebin-paste-get-attr paste 'paste_key)
                         "&api_option=delete"))
         (url-request-method "POST")
         (url-request-extra-headers
          '(("Content-Type" . "application/x-www-form-urlencoded")))
         (url-request-data params)
         (content-buf (url-retrieve
                       pastebin-post-request-paste-url
                       (lambda (arg &rest paste)
                         (cond
                          ((equal :error (car arg))
                           (signal 'pastebin-error (cdr arg)))
                          (t
                           (message (format "Paste %s deleted" (pastebin-paste-get-attr paste 'paste_title)))))) 
                       paste)))))

(defun pastebin-create-unused-buffer-name (name)
  "Create a uniq buffer name, based on `name'<N>"
  (let ((bname name)
        (i 0))
    (when (get-buffer bname)
      (setq bname (format "%s<%d>" bname (setq i (1+ i))))
      
      (while (get-buffer bname)
        (setq bname (replace-regexp-in-string "<[0-9]+>" (format "<%d>" (setq i (1+ i))) bname))))
    bname))

;; FIXME: Set modes acordily to pastebin-type-assoc and update pastebin-pastes-list
;; Also handle the (buffer already in use) problem 
;; And handle the danm ^M characters
(defun pastebin-paste-fetch (paste)
  "Fetch `paste' from pastebin.com, create a buffer and switch to it"
  (let* ((paste-url (concat "http://pastebin.com/raw.php?i=" (pastebin-paste-get-attr paste 'paste_key)))
         (url-request-method "POST")
         (url-request-extra-headers
          '(("Content-Type" . "application/x-www-form-urlencoded")))
         (content-buf (url-retrieve 
                       paste-url
                       (lambda (arg &rest paste)
                         (cond
                          ((equal :error (car arg))
                           (signal 'pastebin-error (cdr arg)))
                          (t
                           (re-search-forward "\n\n")
                           (kill-region (point-min) (point))
                           (while (re-search-forward "\r" nil t)
                             (replace-match ""))
                           (goto-char (point-min))
                           (let ((paste-name (or (pastebin-paste-get-attr paste 'paste_title) 
                                                 "_UNTITLED_")))
                             (rename-buffer (pastebin-create-unused-buffer-name paste-name)))
                           (switch-to-buffer-other-window (current-buffer)))))
                       paste)))))



;; FIXME: I want to support deletion list from here. Untitled pastes
;; should be highlighted! Maybe I can retitle the paste from here?
(defun pastebin-list-buffer ()
  "Create a buffer with a list of pastes and switch to it
Some keybinds are setted"
  (interactive)
  (pastebin-login-if-not-already)
  (and (pastebin-pastes-list-expired?)
       (pastebin-pastes-list-fetch))

  (while (not pastebin-pastes-list)
    (sleep-for 0.2))

  (with-current-buffer (get-buffer-create "*Pastebin List*")
    (let ((inhibit-read-only t))
      (erase-buffer))

    (widget-minor-mode 1)
    (use-local-map pastebin-map)

    (widget-insert (format "%5.5s | %-8.8s | %-32.32s | %-7.7s | %-15.15s\n"
                           "VIEW" "ID" "TITLE" "SYNTAX" "DATE"))
    (dolist (paste (pastebin-pastes))
      (widget-create 'link 
                     :notify (lambda (wid &rest ignore)
                               (pastebin-paste-fetch (widget-get wid :paste)))
                     :paste paste
                     :follow-link t
                     :value (format "%4.4s | %-8.8s | %-32.32s | %-7.7s | %-20.20s"
                                    (if (string= (pastebin-paste-get-attr paste 'paste_private) "1")
                                        "PRIV"
                                      "PUBL")
                                    (pastebin-paste-get-attr paste 'paste_key)
                                    (or (pastebin-paste-get-attr paste 'paste_title) "")
                                    (pastebin-paste-get-attr paste 'paste_format_short)
                                    (format-time-string "%c" (seconds-to-time (string-to-number (pastebin-paste-get-attr paste 'paste_date))))
                                    )
                     )

      (widget-insert "\n")
      )
    (widget-setup)
    (goto-char (point-min))
    (switch-to-buffer (current-buffer))
    )
  )


(provide 'pastebin)
;;; pastebin.el ends herex
