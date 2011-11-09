# copy .ssh directory or generate keys

sudo apt-get install aptitude git subversion openssh-server screen

git clone comstocl@dgtized.net:git/home-config.git .home-config
~/.home-config/setup.sh debian

sudo aptitude install build-essential

# emacs
sudo add-apt-repository ppa:cassou/emacs
sudo aptitude install emacs-snapshot-{gtk,el}

~/.home-config/setup.sh up

# packages: libm17n-dev libgnutls-dev libgconf2-dev

# java
sudo add-apt-repository ppa:ferramroberto/java
sudo apt-get update
sudo aptitude install sun-java6-jdk sun-java6-fonts sun-java6-plugin
sudo update-alternatives --config java

# ruby
sudo aptitude install ruby-full
wget http://production.cf.rubygems.org/rubygems/rubygems-1.7.2.tgz
tar zxf rubygems-1.7.2.tgz
cd rubygems-1.7.2
sudo ruby setup.rb
sudo gem1.8 update --system
sudo ln -s /usr/bin/gem1.8 /usr/bin/gem

# grails
sudo add-apt-repository ppa:groovy-dev/grails
sudo apt-get update
sudo apt-get install grails

# maven2
sudo aptitude install maven2

# postgres

sudo add-apt-repository ppa:pitti/postgresql
sudo aptitude update
sudo aptitude install postgresql-9.0 libpq-dev pgAdmin3

# to dump all with roles
# pg_dumpall > adjudica.psql.gz
# gzip adjudica.psql
# scp adjudica.psql.gz $host:

# this may be optional if done on the other server
# sudo -u postgres createuser $USER --pwprompt --username=postgres

# restore
# gunzip adjudica.psql.gz
# psql -f adjudica.psql postgres

# git clone git@github.com:adjudicallc/Adjudica.git adjudica
