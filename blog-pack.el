;;; blog-pack.el --- Blog with org-mode and wordpress + credentials setup

;;; Commentary:

;;; Code:

(require 'install-packages-pack)
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

;; activate option to keep the passphrase (it's preferable to use gpg-agent but I did not yet succeeded in doing so)
(setq epa-file-cache-passphrase-for-symmetric-encryption t)

;; credentials using netrc (it can deal with space in entries)
(defvar *BLOG-PACK-CREDENTIALS-FILE* (concat (getenv "HOME") "/.authinfo.gpg"))

;; ===================== setup functions

(defun blog-pack/--setup-possible-p (creds-file)
  "Check if the installation is possible by checking the existence of the file and that the entry 'blog' and 'blog-description' are indeed installed."
  (let ((parsed-file (creds/read-lines creds-file)))
    (when (and parsed-file ;; nil if the file does not exist
               (creds/get parsed-file "blog")
               (creds/get parsed-file "blog-description"))
      parsed-file)))

(defun blog-pack/--setup (creds-file-content)
  "The org2blog setup (no check on the existence of the file)."
  (message (format "Running blog-pack setup..."))
  (let* ((blog             (creds/get creds-file-content "blog"))
         (blog-login       (creds/get-entry blog "login"))
         (blog-pass        (creds/get-entry blog "password")) ;; (wrap the password in " if there is space in it)
         (blog-description (creds/get creds-file-content "blog-description"))
         (blog-name        (creds/get-entry blog-description "blog-name"))
         (blog-url-rpc     (creds/get-entry blog-description "blog-url-rpc")))
    (setq org2blog/wp-blog-alist `((,blog-name
                                    :url ,blog-url-rpc
                                    :username ,blog-login
                                    :password ,blog-pass)))
    (add-hook 'org-mode-hook 'org2blog/wp-mode)
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
    (message "Setup done!")))

;; ===================== setup routine

(defun blog-pack/load-pack! ()
  (interactive)
  "A routine to (re)load the blog-pack."
  (-if-let (creds-file-content (blog-pack/--setup-possible-p *BLOG-PACK-CREDENTIALS-FILE*))
      (blog-pack/--setup creds-file-content)
    (message (concat "You need to setup the credentials file " *BLOG-PACK-CREDENTIALS-FILE* " for this to work.\n"
                     "Here is the needed content to setup to your need into '" *BLOG-PACK-CREDENTIALS-FILE* "':\n"
                     "machine blog login <your-wordpress-login> password <your-wordpress-password-inside-quote>\n"
                     "machine blog-description blog-name <blog-name-you-desire> blog-url-rpc http://<path-to-your-blog-ip-and-folder>/xmlrpc.php\n"
                     "Then `M-x encrypt-epa-file` to generate the required ~/.authinfo.gpg and remove ~/.authinfo"))))

(provide 'blog-pack)
;;; blog-pack.el ends here
