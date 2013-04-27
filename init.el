(install-packs '(;; need org2blog as a glue between org-mode and wordpress
                 xml-rpc
                 org2blog))

;; hack - there is some dep that has been broken since punchagan separated org2blog and metaweblog.el (https://github.com/punchagan/metaweblog.el)
(live-add-pack-lib "metaweblog")
(live-add-pack-lib "creds")

;; depends on netrc, xml-rpc
(require 'org2blog-autoloads)
(require 'netrc)
(require 'metaweblog)
(require 'creds)

;; credentials using netrc (it can deal with space in entries)

;; load the entry tony-blog in the ~/.netrc, we obtain a hash-map with the needed data
(setq blog (netrc-machine (netrc-parse "~/.netrc") "blog" t))

(setq blog-login (netrc-get blog "login"))
(setq blog-pass (netrc-get blog "password"))

;; blog description using creds (no dealing with space at the moment)

(setq blog-description (get-creds (read-lines "~/.netrc") "blog-description"))

;; name of the blog
(setq blog-name (get-entry blog-description "blog-name"))
(setq blog-url-rpc (get-entry blog-description "blog-url-rpc"))

;; now enter the data in the org2blog list of blogs
(setq org2blog/wp-blog-alist
      `((,blog-name
         :url ,blog-url-rpc
         :username ,blog-login
         :password ,blog-pass)))

(add-hook 'org-mode-hook 'org2blog/wp-mode)
