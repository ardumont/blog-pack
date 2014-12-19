;;; blog-pack.el --- Blog with org-mode and jekyll/github

;;; Commentary:

;;; Code:

(require 'install-packages-pack)
(install-packages-pack/install-packs '(org2jekyll))

(require 'org)
(require 'org2jekyll)

(custom-set-variables '(org2jekyll/blog-entry "tony-blog")
                      '(org2jekyll/blog-author "ardumont")
                      '(org2jekyll/source-directory  (expand-file-name "~/org/"))
                      '(org2jekyll/jekyll-directory  (expand-file-name "~/public_html/"))
                      '(org2jekyll/jekyll-drafts-dir "_drafts/")
                      '(org2jekyll/jekyll-posts-dir  "_posts/")
                      '(org-publish-project-alist
                        `(("tony-blog"
                           :base-directory ,(org2jekyll/input-directory)
                           :base-extension "org"
                           :publishing-directory ,(org2jekyll/output-directory "_posts")
                           :publishing-function org-html-publish-to-html
                           :headline-levels 4
                           :section-numbers nil
                           :with-toc nil
                           :html-head "<link rel=\"stylesheet\" href=\"./css/style.css\" type=\"text/css\"/>"
                           :html-preamble t
                           :recursive t
                           :make-index t
                           :html-extension "html"
                           :body-only t)

                          ("images"
                           :base-directory ,(org2jekyll/input-directory "img")
                           :base-extension "jpg\\|gif\\|png"
                           :publishing-directory ,(org2jekyll/output-directory "img")
                           :publishing-function org-publish-attachment)

                          ("js"
                           :base-directory ,(org2jekyll/input-directory "js")
                           :base-extension "js"
                           :publishing-directory ,(org2jekyll/output-directory "js")
                           :publishing-function org-publish-attachment)

                          ("other"
                           :base-directory ,(org2jekyll/input-directory "css")
                           :base-extension "css\\|el"
                           :publishing-directory ,(org2jekyll/output-directory "css")
                           :publishing-function org-publish-attachment)

                          ("website" :components ("org" "tony-blog" "images" "js" "css")))))

(provide 'blog-pack)
;;; blog-pack.el ends here
