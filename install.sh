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
GUILE_JSON_VERSION=4.7.3
GECKODRIVER_VERSION=0.30.0
cd libs
  wget https://download.savannah.gnu.org/releases/guile-json/guile-json-$GUILE_JSON_VERSION.tar.gz
  aunpack guile-json-$GUILE_JSON_VERSION.tar.gz
  cd guile-json-$GUILE_JSON_VERSION
    ./configure --prefix=/usr
    make
    sudo make install
  cd ..
  wget https://github.com/mozilla/geckodriver/archive/refs/tags/v$GECKODRIVER_VERSION.tar.gz \
      --output-document=geckodriver-$GECKODRIVER_VERSION.tar.gz
  aunpack geckodriver-$GECKODRIVER_VERSION.tar.gz
  cd geckodriver-$GECKODRIVER_VERSION
    cargo install --path .
  cd ..
  export PATH=$PATH:~/.cargo/bin
  echo *** Make sure ~/.cargo/bin is in your path to use geckodriver! ***
cd ..

make clean
make
sudo make GUILE=$GUILE install

