!define VERSION "0.5.0"
!include EnvVarUpdate.nsh

# define the name of the installer
outfile "scotch-${VERSION}-installer.exe"
icon "scotch.ico"
 
# define the directory to install to, the desktop in this case as specified  
# by the predefined $DESKTOP variable
installDir "$PROGRAMFILES\Scotch${VERSION}"

DirText "Choose a directory to install Scotch ${VERSION}"
 
# default section
section
 
# define the output path for this file
setOutPath $INSTDIR
 
# define what to install and place it in the output path

file /r /x "*.osc" "installer\*.*"
createShortCut "$SMPROGRAMS\Scotch ${VERSION}\Scotch.lnk" "$INSTDIR\scotch.exe"
${EnvVarUpdate} $0 "PATH" "A" "HKLM" $INSTDIR
AccessControl::GrantOnFile "$INSTDIR\scotch.lib" "(BU)" "FullAccess"
 
sectionEnd