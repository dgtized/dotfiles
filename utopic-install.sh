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
     p7zip-full tree parallel gawk \
     lm-sensors htop iotop linux-tools-common linux-tools-generic \ # perf
     default-jdk \
     gitk \
     httpie awscli \
     gnutls-bin

# uncomment localhost for postgres

sudo apt-get install libxss1 libappindicator1 libindicator7
wget https://dl.google.com/linux/direct/google-chrome-beta_current_amd64.deb
sudo dpkg -i google-chrome*.deb

curl --silent --location https://deb.nodesource.com/setup_5.x | sudo bash - && \
    sudo apt-get install --yes nodejs

# install chruby
wget -O ruby-install-0.6.0.tar.gz https://github.com/postmodern/ruby-install/archive/v0.6.0.tar.gz
tar -xzvf ruby-install-0.6.0.tar.gz
pushd ruby-install-0.6.0/
sudo make install
popd

wget -O chruby-0.3.9.tar.gz https://github.com/postmodern/chruby/archive/v0.3.9.tar.gz
tar -xzvf chruby-0.3.9.tar.gz
pushd chruby-0.3.9/
sudo make install
popd

ruby-install ruby 2.2.3

sudo sed -i.bak '/PasswordAuthentication /c\PasswordAuthentication no' /etc/ssh/sshd_config
sudo service ssh restart

#wget https://github.com/github/hub/releases/download/v2.2.1/hub-linux-amd64-2.2.1.tar.gz
#tar zxf && mv hub ~/usr/bin

# wget https://packages.erlang-solutions.com/erlang-solutions_1.0_all.deb && sudo dpkg -i erlang-solutions_1.0_all.deb
# sudo aptitude update
# sudo aptitude install esl-erlang elixir
# git clone elixir source + download otp for alchemist reference
