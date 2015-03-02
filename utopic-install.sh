sudo apt-get install aptitude git openssh-server tmux build-essential

sudo apt-add-repository ppa:ubuntu-elisp/ppa
sudo aptitude install emacs-snapshot emacs-snapshot-el

sudo aptitude install fonts-inconsolata python-wnck

sudo aptitude install silversearcher-ag ack-grep

sudo aptitude install mysql-client-5.6 mysql-server-5.6 libmysqlclient-dev \
     postgresql-9.4  libpq-dev

# uncomment localhost for postgres

sudo apt-get install git-core curl zlib1g-dev build-essential libssl-dev libreadline-dev libyaml-dev libsqlite3-dev sqlite3 libxml2-dev libxslt1-dev libcurl4-openssl-dev python-software-properties

sudo aptitude install redis-server redis-tools

sudo aptitude install graphviz dconf-cli gnome-tweak-tool

sudo apt-get install libxss1 libappindicator1 libindicator7
wget https://dl.google.com/linux/direct/google-chrome-beta_current_amd64.deb
sudo dpkg -i google-chrome*.deb
