rmdir installer

mkdir installer

ghc --make scotch

copy scotch.exe installer
robocopy scotch.lib installer/scotch.lib /e

makensisw winstaller.nsi

rmdir installer /s /q
