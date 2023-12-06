sudo apt install aptitude git openssh-server tmux build-essential xterm

sudo apt-add-repository ppa:ubuntu-elisp/ppa
sudo aptitude update
sudo apt install emacs-snapshot emacs-snapshot-el vim \
     fonts-inconsolata fonts-dejavu fonts-cascadia-code \
     silversearcher-ag ack exuberant-ctags \
     git curl zlib1g-dev build-essential libssl-dev libreadline-dev libyaml-dev libsqlite3-dev sqlite3 libxml2-dev libxslt1-dev libcurl4-openssl-dev \
     dconf-cli \
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
     default-jdk openjdk-8-jdk openjdk-17-jdk \
     visualvm scala scala-doc \
     golang-go perl-doc gawk gawk-doc \
     python-pip python3-venv python3-doc

# select java version
sudo update-java-alternatives -s java-1.17.0-openjdk-amd64

# install quicklisp after sbcl: https://www.quicklisp.org/beta/

# databases
sudo apt install mysql-client mysql-server libmysqlclient-dev \
     postgresql libpq-dev redis-server redis-tools
# uncomment listen_address=localhost for postgres in postgresql.conf
sudo service postgresql restart

sudo apt install libxss1 libappindicator1 libindicator7
wget https://dl.google.com/linux/direct/google-chrome-beta_current_amd64.deb
sudo dpkg -i google-chrome*.deb

# nodejs, npm, and yarn
curl -sS https://dl.yarnpkg.com/debian/pubkey.gpg | sudo apt-key add -
echo "deb https://dl.yarnpkg.com/debian/ stable main" | sudo tee /etc/apt/sources.list.d/yarn.list

sudo apt-get update
sudo apt-get install -y ca-certificates curl gnupg
sudo mkdir -p /etc/apt/keyrings
curl -fsSL https://deb.nodesource.com/gpgkey/nodesource-repo.gpg.key | sudo gpg --dearmor -o /etc/apt/keyrings/nodesource.gpg

NODE_MAJOR=20
echo "deb [signed-by=/etc/apt/keyrings/nodesource.gpg] https://deb.nodesource.com/node_$NODE_MAJOR.x nodistro main" | sudo tee /etc/apt/sources.list.d/nodesource.list

sudo apt install --yes nodejs nodejs-doc yarn npm

# install chruby
wget -O ruby-install.tar.gz https://github.com/postmodern/ruby-install/archive/v0.9.0.tar.gz &&
    tar -xzvf ruby-install.tar.gz &&
    pushd ruby-install-0.9.0/ &&
    sudo make install &&
    popd &&
    rm -rvf ruby-install.tar.gz ruby-install-0.9.0

wget -O chruby-0.3.9.tar.gz https://github.com/postmodern/chruby/archive/v0.3.9.tar.gz &&
    tar -xzvf chruby-0.3.9.tar.gz &&
    pushd chruby-0.3.9/ &&
    sudo make install &&
    popd

ruby-install ruby 3.2.2

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
# sudo apt install -y chamber
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

# apt install apt-transport-https ca-certificates
# sudo apt-key adv --keyserver hkp://p80.pool.sks-keyservers.net:80 --recv-keys 58118E89F3A912897C070ADBF76221572C52609D
# echo "deb https://apt.dockerproject.org/repo ubuntu-wily main" | \
#    sudo tee /etc/apt/sources.list.d/docker.list
# sudo apt update
# sudo apt install linux-image-extra-$(uname -r)
# sudo apt install docker-engine
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

(export CHROME=https://chromedriver.storage.googleapis.com/94.0.4606.41/chromedriver_linux64.zip &&
     export ZIP="$HOME/Downloads/chromedriver.zip"
     mkdir -p "$HOME/usr/bin" && curl "${CHROME}" -o "${ZIP}" &&
     unzip "${ZIP}" && mv -vf chromedriver "$HOME/usr/bin/chromedriver" && rm -vf "${ZIP}")

# firefox driver
# (export V='v0.33.0' &&
#      export FIREFOX=https://github.com/mozilla/geckodriver/releases/download/${V}/geckodriver-${V}-linux64.tar.gz &&
#      curl -sLO $FIREFOX && tar zxf ${FIREFOX##*/} && mv -vf geckodriver ~/usr/bin && rm -vf ${FIREFOX##*/})
# use the snap version:
ln -sfv /snap/bin/firefox.geckodriver geckodriver

# sudo apt install supercollider supercollider-language pipewire-audio-client-libraries
# sudo apt install patchance python3-qt5 python3-qt5.qtsvg

# sudo apt install haskell-platform haskell-stack

# sudo apt install gnuplot-x11

# sudo echo "deb http://cran.rstudio.com/bin/linux/ubuntu xenial/" | sudo tee -a /etc/apt/sources.list
# gpg --keyserver keyserver.ubuntu.com --recv-key E084DAB9 &&  gpg -a --export E084DAB9 | sudo apt-key add -
# sudo aptitude update && sudo apt install r-base r-base-dev

# screencast to gif tools
sudo add-apt-repository ppa:sethj/silentcast
sudo add-apt-repository ppa:peek-developers/stable
sudo aptitude update
sudo aptitude install peek silentcast

# wget https://github.com/adobe-fonts/source-code-pro/archive/2.030R-ro/1.050R-it.tar.gz
#

curl --output clj.sh -O https://download.clojure.org/install/linux-install-1.11.1.1413.sh &&
    chmod +x clj.sh && sudo ./clj.sh && rm -vf clj.sh

curl -L -O https://github.com/clojure/brew-install/releases/latest/download/linux-install.sh && chmod +x linux-install.sh && sudo ./linux-install.sh && clj --version

curl -sLO https://raw.githubusercontent.com/borkdude/clj-kondo/master/script/install-clj-kondo &&
    chmod +x install-clj-kondo && ./install-clj-kondo --dir "$HOME/usr/bin" &&
    rm -vf install-clj-kondo

# babashka
# download and unpack from https://github.com/borkdude/babashka/releases
(export BV="1.3.185" &&
     wget https://github.com/babashka/babashka/releases/download/v${BV}/babashka-${BV}-linux-amd64.tar.gz -O babashka.tgz) &&
    tar -zxf babashka.tgz && mv -vf bb ~/usr/bin && rm -vf babashka.tgz

# boot
bash -c "cd $HOME/usr/bin && curl -fsSLo boot https://github.com/boot-clj/boot-bin/releases/download/latest/boot.sh && chmod 755 boot" && boot

# Emacs Eldev

curl -fsSL https://raw.github.com/emacs-eldev/eldev/master/bin/eldev > ~/usr/bin/eldev && chmod a+x ~/usr/bin/eldev

# LaTeX
# apt install texlive-full

# ripgrep
(export v='13.0.0' && export d="ripgrep_${v}_amd64.deb" &&
     curl -LO "https://github.com/BurntSushi/ripgrep/releases/download/${v}/${d}" &&
     sudo dpkg -i "${d}" && rm -vf "${d}")

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
# see https://help.ubuntu.com/community/DigitalAC-3Pulseaudio for 5.1
sudo apt install pavucontrol

# inkscape / plotter
sudo add-apt-repository ppa:inkscape.dev/stable
sudo apt update
sudo apt install inkscape

# protontricks
# https://simpler-website.pages.dev/html/2021/1/protontricks/
sudo apt install -y python3-pip python3-setuptools python3-venv
python3 -m pip install --user pipx
python3 -m pipx ensurepath

pipx install protontricks

# ESP32
# install vs-code + platformio
# https://docs.platformio.org/en/latest/core/installation/udev-rules.html
# https://docs.espressif.com/projects/esp-idf/en/latest/esp32/get-started/linux-macos-setup.html
# on conflict with brltty
# https://unix.stackexchange.com/questions/696001/dev-ttyusb0-is-available-but-after-try-to-call-its-gone
# sudo apt remove brltty
# test that /dev/ttyUSB0 exists after plugging in device

# wat2wasm + friends
sudo apt install wabt

# use gnome-session-properties to add script/gnome-xinit.sh at startup
