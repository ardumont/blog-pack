blog-pack
=========

A pack to help in setup `org2blog` to use with one's wordpress blog.

# install

In your `.emacs-live.el` add this snippet:
```elisp
(live-add-packs '(blog-pack))
```

# Setup

Create a `~/.netrc` file with your wordpress credentials your blog description:

## credentials

In a line, add your wordpress credentials in a `machine blog` line:
```txt
machine blog login some-login password some-password
```

The `machine blog` is static, do not change it.

Example:
```txt
machine blog login tony password "this is a password for my blog"
```

*Note* If you have space in your password, indeed quote the password with ".

## description

In another line, add the `machine blog-description` entry like this:
```txt
machine blog-description blog-name your-blog-name blog-url-rpc http://your-wordpress-blog.com/blog/xmlrpc.php
```

Example:
```txt
machine blog-description blog-name tony-blog blog-url-rpc http://tony-blog.fr/blog/xmlrpc.php
```

The `machine blog-description` is static too, so do not change it.

Ok, now you're all ready to go.

# run

Start emacs.
Open a org-mode file and write whatever you wish.

When ready to publish a draft, hit `C-t d`.
When ready to publish, hit `C-t p`.
