;;; blog-pack.el --- Blog with org-mode and wordpress + credentials setup

;;; Commentary:

;;; Code:

(install-packs '(;; need org2blog as a glue between org-mode and wordpress
                 xml-rpc
                 org2blog
                 creds
                 org))

;; ===================== lib

(require 'org)
(require 'netrc)
(require 'creds)
(require 'org2blog-autoloads)

;; ===================== setup file

;; credentials using netrc (it can deal with space in entries)
(defvar *BLOG-PACK-CREDENTIALS-FILE* (concat (getenv "HOME") "/.authinfo.gpg"))

;; ===================== setup functions

(defun blog-pack/--setup-possible-p (creds-file)
  "Check if the installation is possible by checking the existence of the file and that the entry 'blog' and 'blog-description' are indeed installed."
  (let ((parsed-file (netrc-parse creds-file)))
    (when (and parsed-file ;; nil if the file does not exist
               (netrc-machine parsed-file "blog")
               (netrc-machine parsed-file "blog-description"))
      parsed-file)))

(defun blog-pack/--setup (creds-file creds-file-content)
  "The org2blog setup (no check on the existence of the file)."

  ;; load the entry tony-blog in the ~/.netrc, we obtain a hash-map with the needed data
  (setq blog (netrc-machine creds-file-content "blog" t))

  (setq blog-login (netrc-get blog "login"))
  (setq blog-pass (netrc-get blog "password"))

  ;; blog description using creds (wrap the password in " if there is space in it)

  (setq blog-description (creds/get creds-file-content "blog-description"))

  ;; name of the blog
  (setq blog-name (creds/get-entry blog-description "blog-name"))
  (setq blog-url-rpc (creds/get-entry blog-description "blog-url-rpc"))

  ;; now enter the data in the org2blog list of blogs
  (setq org2blog/wp-blog-alist `((,blog-name
                                  :url ,blog-url-rpc
                                  :username ,blog-login
                                  :password ,blog-pass)))

  (add-hook 'org-mode-hook 'org2blog/wp-mode))

(add-hook 'org2blog/wp-mode-hook
          (lambda ()
            ;; undefine the existing binding
            (define-key org2blog/wp-entry-mode-map (kbd "C-c p") nil)
            (define-key org2blog/wp-entry-mode-map (kbd "C-c P") nil)
            (define-key org2blog/wp-entry-mode-map (kbd "C-c d") nil)
            (define-key org2blog/wp-entry-mode-map (kbd "C-c D") nil)
            (define-key org2blog/wp-entry-mode-map (kbd "C-c t") nil)

            (local-set-key (kbd "C-c b p") 'org2blog/wp-post-buffer-and-publish)
            (local-set-key (kbd "C-c b P") 'org2blog/wp-post-buffer-as-page-and-publish)
            (local-set-key (kbd "C-c b d") 'org2blog/wp-post-buffer)
            (local-set-key (kbd "C-c b D") 'org2blog/wp-post-buffer-as-page)
            (local-set-key (kbd "C-c b t") 'org2blog/wp-complete-category)))

;; ===================== setup routine

(-if-let (creds-file-content (blog-pack/--setup-possible-p *BLOG-PACK-CREDENTIALS-FILE*))
    (progn
      (message (concat *BLOG-PACK-CREDENTIALS-FILE* " found! Running org2blog setup..."))
      (blog-pack/--setup *BLOG-PACK-CREDENTIALS-FILE* creds-file-content)
      (message "Setup done!"))
  (progn
    (message (concat "You need to setup the credentials file " *BLOG-PACK-CREDENTIALS-FILE* " for this to work.\n"
                     "Here is the needed content to setup to your need into '" *BLOG-PACK-CREDENTIALS-FILE* "':\n"
                     "machine blog login <your-wordpress-login> password <your-wordpress-password-inside-quote>\n"
                     "machine blog-description blog-name <blog-name-you-desire> blog-url-rpc http://<path-to-your-blog-ip-and-folder>/xmlrpc.php\n"))))

;;; blog-pack.el ends here
