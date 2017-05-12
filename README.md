A collection of dotfiles that I have accumulated over many years.

### Install

    $ git clone git@github.com:dgtized/dotfiles.git ~/.home-config
    $ ~/.home-config/setup.sh debian

### Secrets

<dl>

<dt>emacs</dt><dd>Symlink or create <code>~/.emacs.d/secrets.el{.gpg}</code>
for local emacs config (see load-secrets in <a href
="https://github.com/dgtized/dotfiles/blob/master/site-lisp/clgc-functions.el">clgc-functions.el</a>).
</dd>

<dt>bash</dt><dd>symlink or create `~/.bashrc.local` and it will be sourced at
start for environment variables and functions.</dd>

<dt>gitconfig</dt><dd>Symlink or create `~/.gitconfig.private`</dd>

</dl>

## License

Distributed under the BSD License, see LICENSE file
