echo "verifying/installing dependencies"
sudo apt-get install automake autoconf wget unzip

echo "fetching source files"
wget "https://github.com/haikarainen/light/archive/master.zip"
unzip master.zip

cd light-master
echo "building light"
./autogen.sh
./configure --with-udev && make
sudo make install
cd ..

echo "removing source files"
rm -rf light-master
rm master.zip
