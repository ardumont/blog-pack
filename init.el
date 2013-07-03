(install-packs '(;; need org2blog as a glue between org-mode and wordpress
                 xml-rpc
                 org2blog
                 metaweblog))

;; ===================== lib deps

;; depends on netrc, xml-rpc
(require 'org2blog-autoloads)
(require 'netrc)
(require 'creds)

;; ===================== setup file

;; credentials using netrc (it can deal with space in entries)
(setq blog-pack--credentials-file (concat (getenv "HOME") "/.netrc"))

;; ===================== setup functions

(defun blog-pack--setup-possible-p (creds-file)
  "Check if the installation is possible by checking the existence of the file and that the entry 'blog' and 'blog-description' are indeed installed."
  (let ((parsed-file (netrc-parse creds-file)))
    (and parsed-file ;; nil if the file does not exist
         (netrc-machine parsed-file "blog")
         (netrc-machine parsed-file "blog-description"))))

(defun blog-pack--setup (creds-file)
  "The org2blog setup (no check on the existence of the file)."
  ;; hack - there is some dep that has been broken since punchagan separated org2blog and metaweblog.el (https://github.com/punchagan/metaweblog.el)
  (live-add-pack-lib "metaweblog")
  (live-add-pack-lib "creds")

  ;; load the entry tony-blog in the ~/.netrc, we obtain a hash-map with the needed data
  (setq blog (netrc-machine (netrc-parse creds-file) "blog" t))

  (setq blog-login (netrc-get blog "login"))
  (setq blog-pass (netrc-get blog "password"))

  ;; blog description using creds (no dealing with space at the moment)

  (setq blog-description (get-creds (read-lines creds-file) "blog-description"))

  ;; name of the blog
  (setq blog-name (get-entry blog-description "blog-name"))
  (setq blog-url-rpc (get-entry blog-description "blog-url-rpc"))

  ;; now enter the data in the org2blog list of blogs
  (setq org2blog/wp-blog-alist
        `((,blog-name
           :url ,blog-url-rpc
           :username ,blog-login
           :password ,blog-pass)))

  (add-hook 'org-mode-hook 'org2blog/wp-mode))

;; ===================== setup routine

(if (blog-pack--setup-possible-p blog-pack--credentials-file)
    (progn
      (message (concat blog-pack--credentials-file " found! Running org2blog setup..."))
      (blog-pack--setup blog-pack--credentials-file)
      (message "Setup done!"))
  (progn
    (message (concat "You need to setup the credentials file " blog-pack--credentials-file " for this to work.\n"
                     "Here is the needed content to setup to your need into '" blog-pack--credentials-file "':\n"
                     "machine blog login <your-wordpress-login> password <your-wordpress-password-inside-quote>\n"
                     "machine blog-description blog-name <blog-name-you-desire> blog-url-rpc http://<path-to-your-blog-ip-and-folder>/xmlrpc.php\n"))))
