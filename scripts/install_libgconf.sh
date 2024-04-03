#!/bin/bash

# from https://2h3ph3rd.medium.com/how-to-install-libgconf-2-4-on-ubuntu-23-10-fec6bda8d5f5

# Downloading packages
wget http://ftp.us.debian.org/debian/pool/main/g/gconf/gconf2_3.2.6-8_amd64.deb
wget http://ftp.us.debian.org/debian/pool/main/g/gconf/libgconf-2-4_3.2.6-8_amd64.deb
wget http://ftp.us.debian.org/debian/pool/main/g/gconf/gconf2-common_3.2.6-8_all.deb
wget http://ftp.us.debian.org/debian/pool/main/g/gconf/gconf-service_3.2.6-8_amd64.deb
wget http://ftp.us.debian.org/debian/pool/main/o/openldap/libldap-2.5-0_2.5.13%2bdfsg-5_amd64.deb

# Configuring the libgconf-2-4 package
sudo dpkg --configure -a

# Installing the libldap-2.5-0 package
sudo apt-get install libldap-2.5-0

# Installing the gconf packages
sudo dpkg -i gconf2_3.2.6-8_amd64.deb
sudo dpkg -i libgconf-2-4_3.2.6-8_amd64.deb
sudo dpkg -i gconf2-common_3.2.6-8_all.deb
sudo dpkg -i gconf-service_3.2.6-8_amd64.deb

# Resolving any potential dependency issues
sudo apt-get -f install
