#!/bin/sh

set -x
set -e

export GUILE=guile

if pacman --version; then
  sudo pacman -Syu --needed wget atool guile chromium rust firefox
fi
if dnf --version; then
  sudo dnf install wget atool guile22 chromium chromedriver cargo firefox
  export GUILE=guile2.2
  echo *** To use guile-web-driver with fedora, use command guile2.2 instead of guile! ***
fi
mkdir -p libs
cd libs
  wget https://download.savannah.gnu.org/releases/guile-json/guile-json-0.3.1.tar.gz
  aunpack guile-json-0.3.1.tar.gz
  cd guile-json-0.3.1
    ./configure --prefix=/usr
    make
    sudo make install
  cd ..
  wget https://github.com/mozilla/geckodriver/archive/refs/tags/v0.30.0.tar.gz \
      --output-document=geckodriver-0.30.0.tar.gz
  aunpack geckodriver-0.30.0.tar.gz
  cd geckodriver-0.30.0
    cargo install --path .
  cd ..
  export PATH=$PATH:~/.cargo/bin
  echo *** Make sure ~/.cargo/bin is in your path to use geckodriver! ***
cd ..

make
sudo make GUILE=$GUILE install

