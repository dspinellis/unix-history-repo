#!/bin/sh
#
MHRC=$HOME/.mhrc
MAILRC=$HOME/.mailrc

if [ ! -f $MHRC ]
    then
	echo You do not have a .mhrc file.  $MAILRC not clobbered.
	exit 1
    else
	echo Creating $MAILRC from $MHRC ...
    fi

echo "set ask append dot metoo hold autoprint crt=24
# This file automatically generated from $MHRC
#	Created  by  ${USER}@`hostname`  `date`
" > $MAILRC

sed	-e 's/^;/# /'				\
	-e 's/[<>,:;]//g'			\
	-e 's/^[a-zA-Z]/alias	&/'		\
	    < $MHRC >> $MAILRC
