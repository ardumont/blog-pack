# blog-pack

This is a pack to help in setup org2blog to use with one's blog

# install

In your `.emacs-live.el` add this snippet:
```elisp
(live-add-packs '(blog-pack))
```

# Setup

Create a `~/.netrc` file with your wordpress credentials:
```txt
machine some-dummy-machine login some-login password some-password
```

Then modify the `init.el` like this:

```elisp
(setq blog (netrc-machine (netrc-parse "~/.netrc") "some-dummy-machine" t))

(setq org2blog/wp-blog-alist
      '(("some-dummy-machine"
         :url "http://your-blog.fr/blog/xmlrpc.php"
         :username (netrc-get blog "login")
         :password (netrc-get blog "password"))))

```
