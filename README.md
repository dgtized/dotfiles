A collection of dotfiles that I have accumulated over many years.

## Install

    $ git clone git@github.com:dgtized/dotfiles.git ~/.home-config
    $ ~/.home-config/setup.sh debian

## SSH Keys

Follow [ssh-keygen](https://www.ssh.com/academy/ssh/keygen) or [github](https://docs.github.com/en/authentication/connecting-to-github-with-ssh/generating-a-new-ssh-key-and-adding-it-to-the-ssh-agent) to generate a new ssh key, but the following should be sufficient:

    $ ssh-keygen -t ed25519

And then add to github to enable checkout & `echo` into `~/.ssh/authorized_keys` on other hosts.

## Secrets

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
