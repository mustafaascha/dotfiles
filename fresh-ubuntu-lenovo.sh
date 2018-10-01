#!/bin/bash


# add rstudio cran
sudo sh -c 'echo "deb http://cran.rstudio.com/bin/linux/ubuntu trusty/" >> /etc/apt/sources.list'
gpg --keyserver keyserver.ubuntu.com --recv-key E084DAB9
gpg -a --export E084DAB9 | sudo apt-key add -

sudo sh -c 'echo "deb http://linux.dropbox.com/ubuntu xenial main" >> /etc/apt/sources.list'
sudo apt-key adv --keyserver pgp.mit.edu --recv-keys 1C61A2656FB57B7E4DE0F4C1FC918B335044912E

if [-d "convenience" ]; then
  mkdir convenience
fi

# get software
sudo apt-get update
sudo apt-get upgrade
sudo apt-get install -y r-base-core r-base-dev gnome-tweak-tool vim tmux gdebi libxml2-dev libssl-dev libcurl4-openssl-dev libopenblas-dev r-base r-base-dev dpkg gdebi-core libpoppler-cpp-dev default-jre default-jdk r-cran-rjava python-pip python3-pip libfreetype6-dev dropbox nautilus-dropbox git

# SEE: https://www.rstudio.com/products/rstudio/download-server/
wget https://download1.rstudio.org/rstudio-1.1.456-amd64.deb
sudo gdebi rstudio-1.1.456-amd64.deb

# CREDIT TO: https://askubuntu.com/questions/1032842/r-rstudio-installation-script-for-ubuntu-18-04-tdd-packages
# for package suggestions

wget https://gist.githubusercontent.com/brianly/4da4948c43918f0b349d/raw/39bf1a66f8ff03a79aaf9302ae40387b5e886537/crunchbuntu.sh

bash crunchbuntu.sh

