blog-pack
=========

# Overview

A pack to wrap the setuping of [org2blog](https://github.com/punchagan/org2blog), an excellent [org-mode](http://orgmode.org/) to [wordpress](http://wordpress.org/) mode for [emacs](https://www.gnu.org/software/emacs/).

This is licensed under [GPLv3](http://gplv3.fsf.org/).

# Prerequisites

Install org-mode from your package manager

```sh
sudo aptitude install -y org-mode
```

# Install

In your `.emacs-live.el` add this snippet:
```elisp
(live-add-packs '("/path/to/blog-pack"))
```

# Setup

Create a `~/.netrc` file with your wordpress credentials your blog description:

## Credentials

In a line, add your wordpress credentials in a `machine blog` line:
```
machine blog login some-login password some-password
```

The `machine blog` is static, do not change it.

Example:
```
machine blog login tony password "this is a password for my blog"
```

*Note* If you have space in your password, indeed quote the password with ".

## Description

In another line, add the `machine blog-description` entry like this:
```
machine blog-description blog-name your-blog-name blog-url-rpc http://your-wordpress-blog.com/blog/xmlrpc.php
```

Example:
```
machine blog-description blog-name tony-blog blog-url-rpc http://tony-blog.fr/blog/xmlrpc.php
```

The `machine blog-description` is static too, so do not change it.

Ok, now you're all ready to go.

# Run

Start emacs.
Open a org-mode file and write whatever you wish.

When ready to publish a draft, hit `C-t d`.
When ready to publish, hit `C-t p`.

# More

- [demo](http://www.youtube.com/watch?feature=player_detailpage&v=qTYCFu_NEFM#t=4s)
- [article](http://adumont.fr/blog/emacs-live-pack-blog-pack/)
