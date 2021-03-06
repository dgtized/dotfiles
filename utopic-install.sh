sudo apt-get install aptitude git openssh-server tmux build-essential xterm

sudo apt-add-repository ppa:ubuntu-elisp/ppa
sudo aptitude update
sudo apt install emacs-snapshot emacs-snapshot-el vim \
     fonts-inconsolata fonts-dejavu \
     silversearcher-ag ack exuberant-ctags \
     git curl zlib1g-dev build-essential libssl-dev libreadline-dev libyaml-dev libsqlite3-dev sqlite3 libxml2-dev libxslt1-dev libcurl4-openssl-dev \
     dconf-cli gnome-tweak-tool \
     p7zip-full tree parallel pv netcat-openbsd jq \
     wordnet \
     lm-sensors htop iotop mytop linux-tools-common linux-tools-generic valgrind \
     gitk httpie xclip \
     awscli gnutls-bin \
     recordmydesktop mplayer mpv imagemagick gifsicle vlc \
     gnome-startup-applications \
     mlocate mailutils mutt # local, so cron can report errors

# mpv allows generating movies from PPM frames as described in https://nullprogram.com/blog/2020/06/29/

# languages & documentation
sudo apt install rlwrap shellcheck graphviz \
     racket guile-3.0 guile-3.0-doc sbcl sbcl-doc swi-prolog \
     default-jdk visualvm openjdk-8-jdk scala scala-doc \
     golang-go perl-doc gawk gawk-doc \
     python-pip python3-venv python3-doc

# install quicklisp after sbcl: https://www.quicklisp.org/beta/

# databases
sudo apt install mysql-client mysql-server libmysqlclient-dev \
     postgresql libpq-dev redis-server redis-tools
# uncomment listen_address=localhost for postgres in postgresql.conf
sudo service postgresql restart

sudo apt-get install libxss1 libappindicator1 libindicator7
wget https://dl.google.com/linux/direct/google-chrome-beta_current_amd64.deb
sudo dpkg -i google-chrome*.deb

# nodejs, npm, and yarn
curl -sS https://dl.yarnpkg.com/debian/pubkey.gpg | sudo apt-key add -
echo "deb https://dl.yarnpkg.com/debian/ stable main" | sudo tee /etc/apt/sources.list.d/yarn.list

curl -sL https://deb.nodesource.com/setup_15.x | sudo -E bash - &&
    sudo apt install --yes nodejs nodejs-doc yarn

# install chruby
wget -O ruby-install-0.7.0.tar.gz https://github.com/postmodern/ruby-install/archive/v0.7.0.tar.gz &&
    tar -xzvf ruby-install-0.7.0.tar.gz &&
    pushd ruby-install-0.7.0/ &&
    sudo make install &&
    popd

wget -O chruby-0.3.9.tar.gz https://github.com/postmodern/chruby/archive/v0.3.9.tar.gz &&
    tar -xzvf chruby-0.3.9.tar.gz &&
    pushd chruby-0.3.9/ &&
    sudo make install &&
    popd

ruby-install ruby 2.6.6

gem install gist

sudo sed -i.bak '/PasswordAuthentication /c\PasswordAuthentication no' /etc/ssh/sshd_config
sudo service ssh restart

#wget https://github.com/github/hub/releases/download/v2.2.9/hub-linux-amd64-2.2.9.tgz
#tar zxf && mv hub ~/usr/bin

# terraform
version=0.11.11
zip=terraform_${version}_linux_amd64.zip
wget https://releases.hashicorp.com/terraform/$version/$zip &&
    unzip $zip && rm $zip && mv terraform ~/usr/bin && chmod +x ~/usr/bin/terraform
# visit https://github.com/gruntwork-io/terragrunt/releases
wget https://github.com/gruntwork-io/terragrunt/releases/download/v0.17.4/terragrunt_linux_amd64 && \
    mv terragrunt_linux_amd64 ~/usr/bin/terragrunt && chmod +x ~/usr/bin/terragrunt

# chamber
# curl -s https://packagecloud.io/install/repositories/segment/chamber/script.deb.sh | sudo bash
# sudo apt-get install -y chamber
version=v2.3.2
curl -LOs https://github.com/segmentio/chamber/releases/download/${version}/chamber-${version}-linux-amd64 &&
    mv chamber-${version}-linux-amd64 ~/usr/bin/chamber && chmod +x ~/usr/bin/chamber


# wget https://packages.erlang-solutions.com/erlang-solutions_1.0_all.deb && sudo dpkg -i erlang-solutions_1.0_all.deb
# sudo aptitude update
# sudo aptitude install esl-erlang elixir erlang-dev erlang-parsetools
# sudo aptitude install inotify-tools

# for alchemist reference
# git clone git@github.com:elixir-lang/elixir.git
# download otp from https://www.erlang.org/downloads
# mix local.hex
# mix archive.install https://github.com/phoenixframework/archives/raw/master/phoenix_new.ez

# apt-get install apt-transport-https ca-certificates
# sudo apt-key adv --keyserver hkp://p80.pool.sks-keyservers.net:80 --recv-keys 58118E89F3A912897C070ADBF76221572C52609D
# echo "deb https://apt.dockerproject.org/repo ubuntu-wily main" | \
#    sudo tee /etc/apt/sources.list.d/docker.list
# sudo apt-get update
# sudo apt-get install linux-image-extra-$(uname -r)
# sudo apt-get install docker-engine
# sudo service docker start

sudo apt install docker.io &&
    sudo systemctl start docker &&
    sudo systemctl enable docker

# golang
mkdir -p $HOME/gocode
go get -u github.com/ddollar/forego
# pup is like jq, but for html with css selectors
go get https://github.com/ericchiang/pup

#rust
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh

# chromedriver
CHROME=https://chromedriver.storage.googleapis.com/81.0.4044.20/chromedriver_linux64.zip
mkdir -p $HOME/usr/bin && wget $CHROME -O $HOME/Downloads/chromedriver.zip &&
    unzip $HOME/Downloads/chromedriver.zip && mv -f chromedriver $HOME/usr/bin/chromedriver

# firefox driver
FIREFOX=https://github.com/mozilla/geckodriver/releases/download/v0.26.0/geckodriver-v0.26.0-linux64.tar.gz
curl -sLO $FIREFOX && tar zxf ${FIREFOX##*/} && mv -f geckodriver ~/usr/bin && rm -f ${FIREFOX##*/}

# sudo add-apt-repository ppa:supercollider/ppa && sudo apt-get update
# sudo apt-get install supercollider

# sudo apt-get install haskell-platform haskell-stack

# sudo apt-get install gnuplot-x11

# sudo echo "deb http://cran.rstudio.com/bin/linux/ubuntu xenial/" | sudo tee -a /etc/apt/sources.list
# gpg --keyserver keyserver.ubuntu.com --recv-key E084DAB9 &&  gpg -a --export E084DAB9 | sudo apt-key add -
# sudo aptitude update && sudo apt-get install r-base r-base-dev

# screencast to gif tools
sudo add-apt-repository ppa:sethj/silentcast
sudo add-apt-repository ppa:peek-developers/stable
sudo aptitude update
sudo aptitude install peek silentcast

# wget https://github.com/adobe-fonts/source-code-pro/archive/2.030R-ro/1.050R-it.tar.gz
#

curl --output clj-linux-install.sh -O \
     https://download.clojure.org/install/linux-install-1.10.2.774.sh &&
    chmod +x clj-linux-install.sh && sudo ./clj-linux-install.sh

curl -sLO https://raw.githubusercontent.com/borkdude/clj-kondo/master/script/install-clj-kondo &&
    chmod +x install-clj-kondo && ./install-clj-kondo --dir $HOME/usr/bin

# babashka
# download and unpack from https://github.com/borkdude/babashka/releases
wget https://github.com/babashka/babashka/releases/download/v0.4.0/babashka-0.4.0-linux-amd64.tar.gz -O babashka.tgz &&
    tar -zxf babashka.tgz && mv -vf bb ~/usr/bin && rm -vf babashka.tgz

# LaTeX
# apt install texlive-full

# ripgrep
curl -LO https://github.com/BurntSushi/ripgrep/releases/download/11.0.1/ripgrep_11.0.1_amd64.deb &&
    sudo dpkg -i ripgrep_11.0.1_amd64.deb

# aws vault
curl -sL https://github.com/99designs/aws-vault/releases/download/v4.3.0/aws-vault-linux-amd64 > ~/usr/bin/aws-vault
chmod +x ~/usr/bin/aws-vault
# aws-vault add default for key setup

sudo systemctl disable cups
sudo systemctl disable avahi-daemon

# install slack, xoom from webpage

# change zoom to a reasonable scaleFactor on HiDPI
# https://superuser.com/questions/1381054/how-to-enable-hidpi-support-on-zoom-us-linux-client
sed -i s/^scaleFactor=.*$/scaleFactor=2/ "$HOME/.config/zoomus.conf"

# install git-lfs
curl -s https://packagecloud.io/install/repositories/github/git-lfs/script.deb.sh | sudo bash
sudo apt install git-lfs

# install gh
sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-key C99B11DEB97541F0
sudo apt-add-repository https://cli.github.com/packages
sudo apt update
sudo apt install gh

# [optional] Linux Audio Setup
sudo apt install pavucontrol

# use gnome-session-properties to add script/gnome-xinit.sh at startup
