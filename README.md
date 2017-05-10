A collection of dotfiles that I have accumulated over many years.

### Install

    $ git clone git@github.com:dgtized/dotfiles.git ~/.home-config
    $ ~/.home-config/setup.sh debian

### Secrets

For emacs:

Symlink or create `~/.emacs.d/secrets.el{.gpg}` for local emacs config (see load-secrets in [clgc-functions.el](https://github.com/dgtized/dotfiles/blob/master/site-lisp/clgc-functions.el)).

For bash:

symlink or create `~/.bashrc.local` and it will be sourced at start for
environment variables and functions.

For gitconfig

Symlink or create `~/.gitconfig.private`
