#!/bin/ksh
#
# Script to set up environment for SDE. Must be sourced.
#
# Revision history:  1.0 TJC 940603
#

echo  "Setting up environment for SDE ...\c"
 
#
# If SDEROOT defined, assume environment variables are inherited.
# Only define them if it isn't. Aliases etc. always defined.
#

if test ${SDEROOT-SDEROOTUnset} = SDEROOTUnset
then

#
# Always put current directory first in search paths
#
   export SDEROOT=/home/zia/sde
   . $SDEROOT/sdeini
fi

echo " done."
