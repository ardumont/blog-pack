(install-packs '(;; need org2blog as a glue between org-mode and wordpress
                 xml-rpc
                 org2blog))

(defun setup-org2blog (creds-file)
  "The org2blog setup (no check on the existence of the file)."
  ;; hack - there is some dep that has been broken since punchagan separated org2blog and metaweblog.el (https://github.com/punchagan/metaweblog.el)
  (live-add-pack-lib "metaweblog")
  (live-add-pack-lib "creds")

  ;; depends on netrc, xml-rpc
  (require 'org2blog-autoloads)
  (require 'netrc)
  (require 'metaweblog)
  (require 'creds)

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

;; credentials using netrc (it can deal with space in entries)
(setq credentials-file (concat (getenv "HOME") "/.netrc"))

(if (file-exists-p credentials-file)
    (progn
      (message (concat credentials-file "found! Setup org2blog..."))
      (setup-org2blog credentials-file)
      (message "Setup done!"))
  (progn
    (message (concat "You need to setup the credentials file " credentials-file " for this to work.\n"
                     "Here is the needed content to setup to your need into '" credentials-file "':\n"
                     "machine blog login <your-wordpress-login> password <your-wordpress-password-inside-quote>\n"
                     "machine blog-description blog-name <blog-name-you-desire> blog-url-rpc http://<path-to-your-blog-ip-and-folder>/xmlrpc.php\n"))))
