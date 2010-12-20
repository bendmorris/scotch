# define the name of the installer
outfile "scotch-0.1-installer.exe"
 
# define the directory to install to, the desktop in this case as specified  
# by the predefined $DESKTOP variable
installDir "$PROGRAMFILES\Scotch0.1"

DirText "Choose a directory to install Scotch 0.1."
 
# default section
section
 
# define the output path for this file
setOutPath $INSTDIR
 
# define what to install and place it in the output path

file /r "installer\*.*"
# icon "scotch.ico"
createShortCut "$SMPROGRAMS\Scotch 0.1.lnk" "$INSTDIR\scotch.exe"
 
sectionEnd
