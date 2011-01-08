sudo rm deb -rf

VER=`scotch "version" -e`
mkdir deb
cd deb
mkdir DEBIAN
mkdir usr
cd usr
mkdir bin
cd ../..
ghc --make scotch
cp scotch deb/usr/bin
cp scotch.lib deb/usr/bin -r
rm deb/usr/bin/scotch.lib/.svn -rf
rm deb/usr/bin/scotch.lib/std/.svn -rf
cp control deb/DEBIAN
dpkg -b deb scotch-lang_${VER}_all.deb
rm deb -rf
