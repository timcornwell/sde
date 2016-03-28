#!/bin/csh -f
#
# SDE initialization file: sets up search path for binaries and
# areas for data. 
#
# NB SDEROOT must be defined before calling this if you don't want the
# default value
#
# C-shell version
#
if (! $?SDEROOT) then
   setenv SDEROOT /home/zia/sde
endif
#
# Defaults: don't change these.
#
setenv ARCH `$SDEROOT/farch`
setenv MACHINE $ARCH
setenv SDEMEM 8
setenv D0 $SDEROOT/data
setenv SDESCR $SDEROOT/scratch
setenv T1 ./
#
# Stuff for running
#
setenv SDEBIN $SDEROOT/bin/$ARCH
setenv SDEPBIN $SDEROOT/pvm/$ARCH
setenv SDELIB $SDEROOT/lib/$ARCH
#
if -e $SDEROOT/var/sdeini.csh.`domainname` then
   source $SDEROOT/var/sdeini.csh.`domainname`
endif
#
if -e $SDEROOT/var/sdeini.csh.`hostname` then
   source $SDEROOT/var/sdeini.csh.`hostname`
endif
#
if -e $HOME/.sdeini.csh then
   source $HOME/.sdeini.csh
endif
#
# Add SDE binaries to path
#
setenv PATH "$PATH":"$SDEROOT/bin/unix":"$SDEROOT/bin/shadows":"$SDEBIN"
#
# Various useful definitions
#
setenv SYSVER $SDEROOT/source/include/sysver.h
setenv SDEINF $SDEROOT/inf/
#
setenv SDETV ./SDEtvfile
#
# Find the PGPLOT fonts
#
if -e $SDELIB/grfont.dat then
   setenv PGPLOT_FONT $SDELIB/grfont.dat
endif

