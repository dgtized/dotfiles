sudo apt-get install aptitude git openssh-server tmux build-essential

sudo apt-add-repository ppa:ubuntu-elisp/ppa
sudo aptitude update
sudo aptitude install emacs-snapshot emacs-snapshot-el vim \
     fonts-inconsolata fonts-dejavu \
     python-wnck silversearcher-ag ack-grep exuberant-ctags \
     mysql-client-5.6 mysql-server-5.6 libmysqlclient-dev \
     postgresql-9.4 libpq-dev redis-server redis-tools \
     git-core curl zlib1g-dev build-essential libssl-dev libreadline-dev libyaml-dev libsqlite3-dev sqlite3 libxml2-dev libxslt1-dev libcurl4-openssl-dev python-software-properties \
     graphviz dconf-cli gnome-tweak-tool \
     p7zip-full tree \
     lm-sensors \
     default-jdk \
     gitk \
     httpie \
     gnutls-bin

# uncomment localhost for postgres

sudo apt-get install libxss1 libappindicator1 libindicator7
wget https://dl.google.com/linux/direct/google-chrome-beta_current_amd64.deb
sudo dpkg -i google-chrome*.deb

curl --silent --location https://deb.nodesource.com/setup_4.x | sudo bash - && \
    sudo apt-get install --yes nodejs

#rbenv install 2.1.1
#rbenv global 2.1.1

sudo sed -i.bak '/PasswordAuthentication /c\PasswordAuthentication no' /etc/ssh/sshd_config
sudo service ssh restart

#wget https://github.com/github/hub/releases/download/v2.2.1/hub-linux-amd64-2.2.1.tar.gz
#tar zxf && mv hub ~/usr/bin
