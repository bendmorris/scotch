rmdir installer

mkdir installer

ghc --make scotch

copy scotch.exe installer
robocopy scotch.lib installer/scotch.lib /e

cd installer
rmdir .svn /s /q
cd scotch.lib
rmdir .svn /s /q
cd std
rmdir .svn /s /q
cd ../../..

makensisw winstaller.nsi

rmdir installer /s /q
