# Installing Scotch

Follow these instructions to install Scotch.

[TOC]

## Windows

Download and run the installer, then run "scotch.exe" or type "scotch" at the
command prompt.

## Mac

We don't yet have a standard Mac distribution; please follow the instructions
under [building from source](#building-from-source).

## Linux (Debian)

After installing the Debian package, use the command "scotch" from the command
line to run scotch.

# Building from Source

You can build Scotch from source using the cabal tool using the command:

    cabal install --bindir=$prefix
    
This will build an executable called scotch.
